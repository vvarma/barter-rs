use crate::data::MarketMeta;
use barter_data::event::{DataKind, MarketEvent};
use barter_integration::model::{instrument::Instrument, Exchange, Market};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Barter example RSI strategy [`SignalGenerator`] implementation.
pub mod example;

/// May generate an advisory [`Signal`] as a result of analysing an input [`MarketEvent`].
pub trait SignalGenerator {
    /// Optionally return a [`Signal`] given input [`MarketEvent`].
    fn generate_signal(&mut self, market: &MarketEvent<DataKind>) -> Option<Signal>;
}

/// Advisory [`Signal`] for a [`Market`] detailing the [`SignalStrength`] associated with each
/// possible [`Decision`]. Interpreted by an [`OrderGenerator`](crate::portfolio::OrderGenerator).
#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
pub struct Signal {
    pub time: DateTime<Utc>,
    pub exchange: Exchange,
    pub instrument: Instrument,
    pub signals: Vec<(Decision, SignalStrength)>,
    /// Metadata propagated from the [`MarketEvent`] that yielded this [`Signal`].
    pub market_meta: MarketMeta,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum EntryType {
    Market,
    Limit { price: f64, expiry: DateTime<Utc> },
}
impl Default for EntryType {
    fn default() -> Self {
        Self::Market
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct ExecutionStrategy {
    pub entry_type: EntryType,
    pub stop_loss: Option<f64>,
    pub take_profit: Option<f64>,
}
impl ExecutionStrategy {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd)]
pub struct ExecutionStrategyBuilder {
    limit: Option<(f64, DateTime<Utc>)>,
    stop_loss: Option<f64>,
    take_profit: Option<f64>,
}
impl ExecutionStrategyBuilder {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn limit(self, price: f64, expiry: DateTime<Utc>) -> Self {
        Self {
            limit: Some((price, expiry)),
            ..self
        }
    }
    pub fn stop_loss(self, price: f64) -> Self {
        Self {
            stop_loss: Some(price),
            ..self
        }
    }
    pub fn take_profit(self, price: f64) -> Self {
        Self {
            take_profit: Some(price),
            ..self
        }
    }
    pub fn build(self) -> ExecutionStrategy {
        ExecutionStrategy {
            entry_type: match self.limit {
                Some((price, expiry)) => EntryType::Limit { price, expiry },
                None => EntryType::Market,
            },
            stop_loss: self.stop_loss,
            take_profit: self.take_profit,
        }
    }
}

/// Describes the type of advisory signal the strategy is endorsing.
#[derive(Copy, Clone, PartialEq, PartialOrd, Debug, Deserialize, Serialize)]
pub enum Decision {
    Long(ExecutionStrategy),
    CloseLong,
    Short(ExecutionStrategy),
    CloseShort,
}

impl Default for Decision {
    fn default() -> Self {
        Self::Long(Default::default())
    }
}

impl Decision {
    /// Determines if a [`Decision`] is Long.
    pub fn is_long(&self) -> bool {
        matches!(self, Decision::Long(_))
    }

    /// Determines if a [`Decision`] is Short.
    pub fn is_short(&self) -> bool {
        matches!(self, Decision::Short(_))
    }

    /// Determines if a [`Decision`] is an entry (long or short).
    pub fn is_entry(&self) -> bool {
        matches!(self, Decision::Short(_) | Decision::Long(_))
    }

    /// Determines if a [`Decision`] is an exit (close_long or close_short).
    pub fn is_exit(&self) -> bool {
        matches!(self, Decision::CloseLong | Decision::CloseShort)
    }
}

/// Strength of an advisory [`Signal`] decision produced by [`SignalGenerator`] strategy.
#[derive(Copy, Clone, PartialEq, PartialOrd, Debug, Deserialize, Serialize)]
pub struct SignalStrength(pub f64);

/// Force exit Signal produced after an [`Engine`](crate::engine::Engine) receives a
/// [`Command::ExitPosition`](crate::engine::Command) from an external source.
#[derive(Clone, Eq, PartialEq, PartialOrd, Debug, Deserialize, Serialize)]
pub struct SignalForceExit {
    pub time: DateTime<Utc>,
    pub exchange: Exchange,
    pub instrument: Instrument,
}

impl<M> From<M> for SignalForceExit
where
    M: Into<Market>,
{
    fn from(market: M) -> Self {
        let market = market.into();
        Self::new(market.exchange, market.instrument)
    }
}

impl SignalForceExit {
    pub const FORCED_EXIT_SIGNAL: &'static str = "SignalForcedExit";

    /// Constructs a new [`Self`] using the configuration provided.
    pub fn new<E, I>(exchange: E, instrument: I) -> Self
    where
        E: Into<Exchange>,
        I: Into<Instrument>,
    {
        Self {
            time: Utc::now(),
            exchange: exchange.into(),
            instrument: instrument.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_return_decision_is_long() {
        let decision = Decision::Long(Default::default());
        assert_eq!(decision.is_long(), true)
    }

    #[test]
    fn should_return_decision_is_not_long() {
        let decision = Decision::Short(Default::default());
        assert_eq!(decision.is_long(), false)
    }

    #[test]
    fn should_return_decision_is_short() {
        let decision = Decision::Short(Default::default());
        assert_eq!(decision.is_short(), true)
    }

    #[test]
    fn should_return_decision_is_not_short() {
        let decision = Decision::Long(Default::default());
        assert_eq!(decision.is_short(), false)
    }

    #[test]
    fn should_return_decision_is_entry() {
        let decision = Decision::Long(Default::default());
        assert_eq!(decision.is_entry(), true)
    }

    #[test]
    fn should_return_decision_is_not_entry() {
        let decision = Decision::CloseLong;
        assert_eq!(decision.is_entry(), false)
    }

    #[test]
    fn should_return_decision_is_exit() {
        let decision = Decision::CloseShort;
        assert_eq!(decision.is_exit(), true)
    }

    #[test]
    fn should_return_decision_is_not_exit() {
        let decision = Decision::Long(Default::default());
        assert_eq!(decision.is_exit(), false)
    }
}
