use std::{
  fmt::{self, Display},
  str::FromStr,
};

use serde::{Deserialize, Serialize};

use nom::{branch::alt, bytes::complete::tag, combinator::value, IResult};

use super::NomErr;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
#[serde(untagged)]
pub enum ValOrRange {
  Value(f64),
  Range { lo: f64, hi: f64 },
}
impl ValOrRange {
  pub fn from_val(val: f64) -> Self {
    Self::Value(val)
  }
  pub fn from_range(lo: f64, hi: f64) -> Self {
    Self::Range { lo, hi }
  }
}
impl Display for ValOrRange {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Self::Value(v) => f.write_fmt(format_args!("{}", v)),
      Self::Range { lo, hi } => f.write_fmt(format_args!("{} {}", lo, hi)),
    }
  }
}
impl From<Vec<f64>> for ValOrRange {
  fn from(v: Vec<f64>) -> Self {
    match v.len() as u8 {
      1 => Self::from_val(v[0]),
      2 => Self::from_range(v[0], v[1]),
      _ => panic!("Wrong vecor size. Actual: {}. Expected: 1 or 2.", v.len()),
    }
  }
}

/// The default value is `UNKNOWNRefPos`
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum SpaceTimeRefPos {
  #[serde(rename = "GEOCENTER")]
  Geocenter,
  #[serde(rename = "BARYCENTER")]
  Barycenter,
  #[serde(rename = "HELIOCENTER")]
  Heliocenter,
  #[serde(rename = "TOPOCENTER")]
  Topocenter,
  #[serde(rename = "GALACTIC_CENTER")]
  GalacticCenter,
  #[serde(rename = "EMBARYCENTER")]
  Embarycenter,
  #[serde(rename = "MOON")]
  Moon,
  #[serde(rename = "MERCURY")]
  Mercury,
  #[serde(rename = "VENUS")]
  Venus,
  #[serde(rename = "MARS")]
  Mars,
  #[serde(rename = "JUPITER")]
  Jupiter,
  #[serde(rename = "SATURN")]
  Saturn,
  #[serde(rename = "URANUS")]
  Uranus,
  #[serde(rename = "NEPTUNE")]
  Neptune,
  #[serde(rename = "PLUTO")]
  Pluto,
  #[serde(rename = "UNKNOWNRefPos")]
  UnknownRefPos,
}
impl SpaceTimeRefPos {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      value(Self::Geocenter, tag("GEOCENTER")),
      value(Self::Barycenter, tag("BARYCENTER")),
      value(Self::Heliocenter, tag("HELIOCENTER")),
      value(Self::Topocenter, tag("TOPOCENTER")),
      value(Self::GalacticCenter, tag("GALACTIC_CENTER")),
      value(Self::Embarycenter, tag("EMBARYCENTER")),
      value(Self::Moon, tag("MOON")),
      value(Self::Mercury, tag("MERCURY")),
      value(Self::Venus, tag("VENUS")),
      value(Self::Mars, tag("MARS")),
      value(Self::Jupiter, tag("JUPITER")),
      value(Self::Saturn, tag("SATURN")),
      value(Self::Uranus, tag("URANUS")),
      value(Self::Neptune, tag("NEPTUNE")),
      value(Self::Pluto, tag("PLUTO")),
      value(Self::UnknownRefPos, tag("UNKNOWNRefPos")),
    ))(input)
  }
}
impl FromStr for SpaceTimeRefPos {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "GEOCENTER" => Ok(Self::Geocenter),
      "BARYCENTER" => Ok(Self::Barycenter),
      "HELIOCENTER" => Ok(Self::Heliocenter),
      "TOPOCENTER" => Ok(Self::Topocenter),
      "GALACTIC_CENTER" => Ok(Self::GalacticCenter),
      "EMBARYCENTER" => Ok(Self::Embarycenter),
      "MOON" => Ok(Self::Moon),
      "MERCURY" => Ok(Self::Mercury),
      "VENUS" => Ok(Self::Venus),
      "MARS" => Ok(Self::Mars),
      "JUPITER" => Ok(Self::Jupiter),
      "SATURN" => Ok(Self::Saturn),
      "URANUS" => Ok(Self::Uranus),
      "NEPTUNE" => Ok(Self::Neptune),
      "PLUTO" => Ok(Self::Pluto),
      "UNKNOWNRefPos" => Ok(Self::UnknownRefPos),
      _ => Err(format!(
        "Unknown RefPos. Actual: \"{}\"; Expected: one of [GEOCENTER, BARYCENTER, HELIOCENTER,
TOPOCENTER, GALACTIC_CENTER, EMBARYCENTER, MOON, MERCURY, VENUS,
MARS, JUPITER, SATURN, URANUS, NEPTUNE, PLUTO, UNKNOWNRefPos].",
        s
      )),
    }
  }
}

impl Display for SpaceTimeRefPos {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::Geocenter => "GEOCENTER",
      Self::Barycenter => "BARYCENTER",
      Self::Heliocenter => "HELIOCENTER",
      Self::Topocenter => "TOPOCENTER",
      Self::GalacticCenter => "GALACTIC_CENTER",
      Self::Embarycenter => "EMBARYCENTER",
      Self::Moon => "MOON",
      Self::Mercury => "MERCURY",
      Self::Venus => "VENUS",
      Self::Mars => "MARS",
      Self::Jupiter => "JUPITER",
      Self::Saturn => "SATURN",
      Self::Uranus => "URANUS",
      Self::Neptune => "NEPTUNE",
      Self::Pluto => "PLUTO",
      Self::UnknownRefPos => "UNKNOWNRefPos",
    })
  }
}
/// The default value is `UNKNOWNRefPos`
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum SpectralRedshiftRefPos {
  #[serde(rename = "GEOCENTER")]
  Geocenter,
  #[serde(rename = "BARYCENTER")]
  Barycenter,
  #[serde(rename = "HELIOCENTER")]
  Heliocenter,
  #[serde(rename = "TOPOCENTER")]
  Topocenter,
  #[serde(rename = "LSR[K]")]
  LsrK,
  #[serde(rename = "LSRD")]
  Lsrd,
  #[serde(rename = "GALACTIC_CENTER")]
  GalacticCenter,
  #[serde(rename = "LOCAL_GROUP_CENTER")]
  LocalGroupCenter,
  #[serde(rename = "EMBARYCENTER")]
  Embarycenter,
  #[serde(rename = "MOON")]
  Moon,
  #[serde(rename = "MERCURY")]
  Mercury,
  #[serde(rename = "VENUS")]
  Venus,
  #[serde(rename = "MARS")]
  Mars,
  #[serde(rename = "JUPITER")]
  Jupiter,
  #[serde(rename = "SATURN")]
  Saturn,
  #[serde(rename = "URANUS")]
  Uranus,
  #[serde(rename = "NEPTUNE")]
  Neptune,
  #[serde(rename = "PLUTO")]
  Pluto,
  #[serde(rename = "UNKNOWNRefPos")]
  UnknownRefPos,
}
impl SpectralRedshiftRefPos {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      value(Self::Geocenter, tag("GEOCENTER")),
      value(Self::Barycenter, tag("BARYCENTER")),
      value(Self::Heliocenter, tag("HELIOCENTER")),
      value(Self::Topocenter, tag("TOPOCENTER")),
      value(Self::LsrK, tag("LSR[K]")),
      value(Self::Lsrd, tag("LSRD")),
      value(Self::GalacticCenter, tag("GALACTIC_CENTER")),
      value(Self::LocalGroupCenter, tag("LOCAL_GROUP_CENTER")),
      value(Self::Embarycenter, tag("EMBARYCENTER")),
      value(Self::Moon, tag("MOON")),
      value(Self::Mercury, tag("MERCURY")),
      value(Self::Venus, tag("VENUS")),
      value(Self::Mars, tag("MARS")),
      value(Self::Jupiter, tag("JUPITER")),
      value(Self::Saturn, tag("SATURN")),
      value(Self::Uranus, tag("URANUS")),
      value(Self::Neptune, tag("NEPTUNE")),
      value(Self::Pluto, tag("PLUTO")),
      value(Self::UnknownRefPos, tag("UNKNOWNRefPos")),
    ))(input)
  }
}
impl FromStr for SpectralRedshiftRefPos {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "GEOCENTER" => Ok(Self::Geocenter),
      "BARYCENTER" => Ok(Self::Barycenter),
      "HELIOCENTER" => Ok(Self::Heliocenter),
      "TOPOCENTER" => Ok(Self::Topocenter),
      "LSR[K]" => Ok(Self::LsrK),
      "LSRD" => Ok(Self::Lsrd),
      "GALACTIC_CENTER" => Ok(Self::GalacticCenter),
      "LOCAL_GROUP_CENTER" => Ok(Self::LocalGroupCenter),
      "EMBARYCENTER" => Ok(Self::Embarycenter),
      "MOON" => Ok(Self::Moon),
      "MERCURY" => Ok(Self::Mercury),
      "VENUS" => Ok(Self::Venus),
      "MARS" => Ok(Self::Mars),
      "JUPITER" => Ok(Self::Jupiter),
      "SATURN" => Ok(Self::Saturn),
      "URANUS" => Ok(Self::Uranus),
      "NEPTUNE" => Ok(Self::Neptune),
      "PLUTO" => Ok(Self::Pluto),
      "UNKNOWNRefPos" => Ok(Self::UnknownRefPos),
      _ => Err(format!(
        "Unknown RefPos. Actual: \"{}\"; Expected: one of [GEOCENTER, BARYCENTER, HELIOCENTER, TOPOCENTER, LSR[K], LSRD, GALACTIC_CENTER, LOCAL_GROUP_CENTER, EMBARYCENTER, MOON, MERCURY, VENUS, MARS, JUPITER, SATURN, URANUS, NEPTUNE, PLUTO, UNKNOWNRefPos].",
        s
      )),
    }
  }
}

impl Display for SpectralRedshiftRefPos {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::Geocenter => "GEOCENTER",
      Self::Barycenter => "BARYCENTER",
      Self::Heliocenter => "HELIOCENTER",
      Self::Topocenter => "TOPOCENTER",
      Self::LsrK => "LSR[K]",
      Self::Lsrd => "LSRD",
      Self::GalacticCenter => "GALACTIC_CENTER",
      Self::LocalGroupCenter => "LOCAL_GROUP_CENTER",
      Self::Embarycenter => "EMBARYCENTER",
      Self::Moon => "MOON",
      Self::Mercury => "MERCURY",
      Self::Venus => "VENUS",
      Self::Mars => "MARS",
      Self::Jupiter => "JUPITER",
      Self::Saturn => "SATURN",
      Self::Uranus => "URANUS",
      Self::Neptune => "NEPTUNE",
      Self::Pluto => "PLUTO",
      Self::UnknownRefPos => "UNKNOWNRefPos",
    })
  }
}
