/// Dummy visitor implementations doing nothing.
/// To be used when we are not interested in a given part (Time, Spoce, Spectra and/or Redshift)
/// of an STC-S string)
pub mod donothing;

/// Visitor simply echoing on stdout the elements it encounters.
/// Can be useful for debugging and understanding how visitors visit the STC structure.
pub mod echo;
