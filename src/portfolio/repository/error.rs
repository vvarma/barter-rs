use thiserror::Error;

/// All errors generated in the barter::portfolio::repository module.
#[derive(Error, Copy, Clone, Debug)]
pub enum RepositoryError {
    #[error("Failed to serialise struct to JSON")]
    JsonSerialisationError,

    #[error("Failed to deserialise JSON to struct")]
    JsonDeserialisationError,

    #[error("Failed to write data to the repository")]
    WriteError,

    #[error("Failed to read data from the repository")]
    ReadError,

    #[error("Failed to delete data from the repository")]
    DeleteError,

    #[error("Failed to retrieve expected data due to it not being present")]
    ExpectedDataNotPresentError,
}
