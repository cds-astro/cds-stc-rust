use std::fmt::{self, Display};

use serde::{Deserialize, Serialize};

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::multispace1,
  combinator::{cut, map, opt},
  multi::many0,
  number::complete::double,
  sequence::{delimited, preceded, tuple},
  IResult,
};

use super::NomErr;

pub mod common;
use crate::common::SpaceTimeRefPos as RefPos;
use common::*;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum Spectral {
  Value(SpecValue),
  Interval(SpecInterval),
}
impl Display for Spectral {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Value(e) => Display::fmt(e, f),
      Self::Interval(e) => Display::fmt(e, f),
    }
  }
}
impl Spectral {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      map(SpecInterval::parse::<E>, Self::Interval),
      map(SpecValue::parse::<E>, Self::Value),
    ))(input)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct SpecValue {
  refpos: Option<RefPos>,
  #[serde(skip_serializing_if = "Option::is_none")]
  value: Option<f64>,
  #[serde(flatten)]
  unit_to_pixsize: UnitToPixsize,
}
impl SpecValue {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag("Spectral"),
        tuple((
          opt(preceded(multispace1, RefPos::parse::<E>)),
          opt(preceded(multispace1, double)),
          UnitToPixsize::parse::<E>,
        )),
      ),
      |(refpos, value, unit_to_pixsize)| Self {
        refpos,
        value,
        unit_to_pixsize,
      },
    )(input)
  }
}
impl Display for SpecValue {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("Spectral")?;
    if let Some(refpos) = &self.refpos {
      f.write_fmt(format_args!(" {}", refpos))?;
    }
    for v in self.value.iter() {
      f.write_fmt(format_args!(" {}", v))?;
    }
    Display::fmt(&self.unit_to_pixsize, f)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct SpecInterval {
  #[serde(skip_serializing_if = "Option::is_none")]
  fillfactor: Option<f64>,
  #[serde(skip_serializing_if = "Option::is_none")]
  refpos: Option<RefPos>,
  #[serde(rename = "lolimit", skip_serializing_if = "Vec::is_empty")]
  lo_limits: Vec<f64>,
  #[serde(rename = "hilimit", skip_serializing_if = "Vec::is_empty")]
  hi_limits: Vec<f64>,
  #[serde(skip_serializing_if = "Option::is_none")]
  spectral: Option<f64>,
  #[serde(flatten)]
  unit_to_pixsize: UnitToPixsize,
}
impl Display for SpecInterval {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("SpectralInterval")?;
    if let Some(fillfactor) = self.fillfactor {
      f.write_fmt(format_args!(" fillfactor {}", fillfactor))?;
    }
    if let Some(refpos) = &self.refpos {
      f.write_fmt(format_args!(" {}", refpos))?;
    }
    for (lo, hi) in self.lo_limits.iter().zip(self.hi_limits.iter()) {
      f.write_fmt(format_args!(" {} {}", lo, hi))?;
    }
    if let Some(spec) = self.spectral {
      f.write_fmt(format_args!(" Spectral {}", spec))?;
    }
    Display::fmt(&self.unit_to_pixsize, f)
  }
}
impl SpecInterval {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag("SpectralInterval"),
        tuple((
          opt(preceded(
            delimited(multispace1, tag("fillfactor"), multispace1),
            cut(double),
          )),
          opt(preceded(multispace1, RefPos::parse::<E>)),
          cut(many0(preceded(multispace1, double))),
          opt(preceded(
            delimited(multispace1, tag("Spectral"), multispace1),
            cut(double),
          )),
          UnitToPixsize::parse::<E>,
        )),
      ),
      |(fillfactor, refpos, lo_hi_limits, spectral, unit_to_pixsize)| Self {
        fillfactor,
        refpos,
        lo_limits: lo_hi_limits.iter().step_by(2).copied().collect(),
        hi_limits: lo_hi_limits.into_iter().skip(1).step_by(2).collect(),
        spectral,
        unit_to_pixsize,
      },
    )(input)
  }
}
