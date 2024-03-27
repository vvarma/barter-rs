use barter_data::{
    event::{DataKind, MarketEvent},
    subscription::candle::Candle,
};
use chrono::Utc;
use serde::{Deserialize, Serialize};

use crate::{
    execution::{ExecutionClient, Fees, FillEvent},
    portfolio::{OrderEvent, OrderType},
};

use super::FillOrExpire;

/// Configuration for constructing a [`SimulatedExecution`] via the new() constructor method.
#[derive(Copy, Clone, PartialEq, PartialOrd, Debug, Default, Deserialize, Serialize)]
pub struct Config {
    /// Simulated fee percentage to be used for each [`Fees`] field in decimal form (eg/ 0.01 for 1%)
    pub simulated_fees_pct: Fees,
}

#[derive(Clone, PartialEq, PartialOrd, Debug, Default, Deserialize, Serialize)]
/// Simulated execution handler that executes [`OrderEvent`]s to generate [`FillEvent`]s via a
/// simulated broker interaction.
pub struct SimulatedExecution {
    fees_pct: Fees,
    pending_orders: Vec<OrderEvent>,
}

impl ExecutionClient for SimulatedExecution {
    fn add_order(&mut self, order: &OrderEvent) {
        self.pending_orders.push(order.clone())
    }

    fn generate_fill(&mut self, market: &MarketEvent<DataKind>) -> Vec<FillOrExpire> {
        if let DataKind::Candle(candle) = &market.kind {
            let exp_or_fills: Vec<(usize, Option<FillEvent>)> = self
                .pending_orders
                .iter()
                .enumerate()
                .filter(|(_i, o)| {
                    o.exchange == market.exchange && o.instrument == market.instrument
                })
                .filter_map(|(i, o)| match o.order_type {
                    OrderType::Market => Some((i, Some(self.fill_market_order(o, candle)))),
                    OrderType::Limit { price, expiry } => {
                        if candle.close_time > expiry {
                            Some((i, None))
                        } else {
                            self.fill_limit_order(o, candle, price)
                                .map(|fill_event| (i, Some(fill_event)))
                        }
                    }
                    OrderType::StopOrProfit {
                        stop_loss,
                        take_profit,
                    } => self
                        .fill_stop_order(o, candle, stop_loss)
                        .or_else(|| self.fill_limit_order(o, candle, take_profit))
                        .map(|f| (i, Some(f))),
                    OrderType::Stop { stop_loss } => self
                        .fill_stop_order(o, candle, stop_loss)
                        .map(|f| (i, Some(f))),
                    OrderType::TakeProfit { take_profit } => self
                        .fill_limit_order(o, candle, take_profit)
                        .map(|f| (i, Some(f))),
                })
                .collect();
            exp_or_fills
                .into_iter()
                .map(|(i, f)| {
                    let order = self.pending_orders.remove(i);
                    match f {
                        Some(f) => FillOrExpire::Fill(f),
                        None => FillOrExpire::Expire(order),
                    }
                })
                .collect()
        } else {
            vec![]
        }
    }
}

impl SimulatedExecution {
    /// Constructs a new [`SimulatedExecution`] component.
    pub fn new(cfg: Config) -> Self {
        Self {
            fees_pct: cfg.simulated_fees_pct,
            pending_orders: vec![],
        }
    }
    fn fill_order_at_midpoint(&self, o: &OrderEvent, midpoint: f64) -> FillEvent {
        let fill_value_gross = midpoint * o.quantity.abs();
        FillEvent {
            time: Utc::now(),
            exchange: o.exchange.clone(),
            instrument: o.instrument.clone(),
            market_meta: o.market_meta,
            decision: o.decision,
            quantity: o.quantity,
            fill_value_gross,
            fees: self.calculate_fees(&fill_value_gross),
        }
    }
    /// Fills an [`OrderEvent`] based on [`Candle`]'s midpoint price
    fn fill_market_order(&self, o: &OrderEvent, candle: &Candle) -> FillEvent {
        let midpoint = (candle.open + candle.close) / 2.0;
        self.fill_order_at_midpoint(o, midpoint)
    }

    /// Fills an [`OrderEvent`] if [`Candle`]'s price matches the limit_price
    fn fill_limit_order(
        &self,
        o: &OrderEvent,
        candle: &Candle,
        limit_price: f64,
    ) -> Option<FillEvent> {
        let candle_min = candle.open.min(candle.close);
        let candle_max = candle.open.max(candle.close);
        if o.quantity > 0.0 && limit_price > candle_min {
            Some((candle_min + candle_max.min(limit_price)) / 2.0)
        } else if o.quantity < 0.0 && limit_price < candle_max {
            Some((candle_max + candle_min.max(limit_price)) / 2.0)
        } else {
            None
        }
        .map(|midpoint| self.fill_order_at_midpoint(o, midpoint))
    }

    fn fill_stop_order(
        &self,
        o: &OrderEvent,
        candle: &Candle,
        stop_price: f64,
    ) -> Option<FillEvent> {
        let candle_min = candle.open.min(candle.close);
        let candle_max = candle.open.max(candle.close);
        // CloseLong
        if o.quantity < 0.0 && candle_min < stop_price {
            Some(candle_min + candle_max.min(stop_price))
        } else if o.quantity > 0.0 && candle_max > stop_price {
            Some(candle_max + candle_min.max(stop_price))
        } else {
            None
        }
        .map(|midpoint| self.fill_order_at_midpoint(o, midpoint))
    }

    /// Calculates the simulated [`Fees`] a [`FillEvent`] will incur, based on the input [`OrderEvent`].
    fn calculate_fees(&self, fill_value_gross: &f64) -> Fees {
        Fees {
            exchange: self.fees_pct.exchange * fill_value_gross,
            slippage: self.fees_pct.slippage * fill_value_gross,
            network: self.fees_pct.network * fill_value_gross,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Add;

    use chrono::Days;

    use super::*;
    use crate::test_util::{market_event_candle, order_event};

    #[test]
    fn should_generate_ok_fill_event_with_valid_order_event_provided() {
        let mut simulated_execution = SimulatedExecution::new(Config {
            simulated_fees_pct: Fees {
                exchange: 0.1,
                slippage: 0.05,
                network: 0.0,
            },
        });

        let market_event = market_event_candle();
        let input_order = OrderEvent {
            quantity: 10.0,
            instrument: market_event.instrument.clone(),
            exchange: market_event.exchange.clone(),
            ..order_event()
        };
        simulated_execution.add_order(&input_order);

        let actual_result = simulated_execution.generate_fill(&market_event);

        let expected_fill_value_gross = 9800.;
        let expected_fees = Fees {
            exchange: 980.,
            slippage: 490.0,
            network: 0.0,
        };
        assert!(actual_result.len() == 1);
        assert!(
            matches!(&actual_result[0], FillOrExpire::Fill(fill) if fill.fill_value_gross==expected_fill_value_gross&&fill.fees==expected_fees )
        );
    }
    #[test]
    fn should_expire_stale_limit_orders() {
        let mut simulated_execution = SimulatedExecution::new(Config {
            simulated_fees_pct: Fees {
                exchange: 0.1,
                slippage: 0.05,
                network: 0.0,
            },
        });
        let market_event = market_event_candle();
        let input_order = OrderEvent {
            order_type: OrderType::Limit {
                price: 100.,
                expiry: Utc::now().checked_sub_days(Days::new(1)).unwrap(),
            },
            instrument: market_event.instrument.clone(),
            exchange: market_event.exchange.clone(),
            ..order_event()
        };
        simulated_execution.add_order(&input_order);
        let actual_result = simulated_execution.generate_fill(&market_event);
        assert!(actual_result.len() == 1);
        assert!(matches!(&actual_result[0], FillOrExpire::Expire(order) if order == &input_order));
    }
    #[test]
    fn should_fill_limit_orders_or_not() {
        let mut simulated_execution = SimulatedExecution::new(Config {
            simulated_fees_pct: Fees {
                exchange: 0.1,
                slippage: 0.05,
                network: 0.0,
            },
        });
        let market_event = market_event_candle();
        let input_order_no_fill = OrderEvent {
            quantity: 10.0,
            order_type: OrderType::Limit {
                price: 100.,
                expiry: Utc::now().add(Days::new(1)),
            },
            instrument: market_event.instrument.clone(),
            exchange: market_event.exchange.clone(),
            ..order_event()
        };
        let input_order_fill = OrderEvent {
            order_type: OrderType::Limit {
                price: 972.,
                expiry: Utc::now().add(Days::new(1)),
            },
            ..input_order_no_fill.clone()
        };
        let expected_fill_value_gross = 9660.;
        let expected_fees = Fees {
            exchange: 966.,
            slippage: 483.0,
            network: 0.0,
        };
        simulated_execution.add_order(&input_order_no_fill);
        simulated_execution.add_order(&input_order_fill);
        let actual_result = simulated_execution.generate_fill(&market_event);
        assert!(actual_result.len() == 1);
        assert!(
            matches!(&actual_result[0], FillOrExpire::Fill(fill) if fill.fill_value_gross==expected_fill_value_gross&&fill.fees==expected_fees)
        );
    }

    #[test]
    fn should_calculate_simulated_fees_correctly() {
        let simulated_execution = SimulatedExecution::new(Config {
            simulated_fees_pct: Fees {
                exchange: 0.5,
                slippage: 0.1,
                network: 0.001,
            },
        });

        let input_fill_value_gross = 100.0;

        let actual_result = simulated_execution.calculate_fees(&input_fill_value_gross);

        let expected = Fees {
            exchange: 50.0,
            slippage: 10.0,
            network: 0.1,
        };

        assert_eq!(actual_result, expected)
    }
}
