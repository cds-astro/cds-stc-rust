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
use common::*;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum Redshift {
  #[serde(rename = "Redshift")]
  Value(RedshiftValue),
  #[serde(rename = "RedshiftInterval")]
  Interval(RedshiftInterval),
}
impl Display for Redshift {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Value(e) => Display::fmt(e, f),
      Self::Interval(e) => Display::fmt(e, f),
    }
  }
}
impl Redshift {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      map(RedshiftInterval::parse::<E>, Self::Interval),
      map(RedshiftValue::parse::<E>, Self::Value),
    ))(input)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct RedshiftValue {
  #[serde(flatten)]
  refpos_to_vals: RefposToVals,
  #[serde(rename = "redshift", skip_serializing_if = "Option::is_none")]
  values: Option<f64>,
  #[serde(flatten)]
  unit_to_pixsize: UnitToPixsize,
}
impl RedshiftValue {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag("Redshift"),
        tuple((
          RefposToVals::parse::<E>,
          opt(preceded(multispace1, double)),
          UnitToPixsize::parse::<E>,
        )),
      ),
      |(refpos_to_vals, values, unit_to_pixsize)| Self {
        refpos_to_vals,
        values,
        unit_to_pixsize,
      },
    )(input)
  }
}
impl Display for RedshiftValue {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("Redshift")?;
    Display::fmt(&self.refpos_to_vals, f)?;
    for v in self.values.iter() {
      f.write_fmt(format_args!(" {}", v))?;
    }
    Display::fmt(&self.unit_to_pixsize, f)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct RedshiftInterval {
  #[serde(skip_serializing_if = "Option::is_none")]
  fillfactor: Option<f64>,
  #[serde(flatten)]
  refpos_to_vals: RefposToVals,
  #[serde(rename = "lolimit", skip_serializing_if = "Vec::is_empty")]
  lo_limits: Vec<f64>,
  #[serde(rename = "hilimit", skip_serializing_if = "Vec::is_empty")]
  hi_limits: Vec<f64>,
  #[serde(rename = "redshift", skip_serializing_if = "Option::is_none")]
  value: Option<f64>,
  #[serde(flatten)]
  unit_to_pixsize: UnitToPixsize,
}
impl Display for RedshiftInterval {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("RedshiftInterval")?;
    if let Some(fillfactor) = self.fillfactor {
      f.write_fmt(format_args!(" fillfactor {}", fillfactor))?;
    }
    Display::fmt(&self.refpos_to_vals, f)?;
    for (lo, hi) in self.lo_limits.iter().zip(self.hi_limits.iter()) {
      f.write_fmt(format_args!(" {} {}", lo, hi))?;
    }
    if let Some(value) = self.value {
      f.write_fmt(format_args!(" Redshift {}", value))?;
    }
    Display::fmt(&self.unit_to_pixsize, f)
  }
}
impl RedshiftInterval {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag("RedshiftInterval"),
        tuple((
          opt(preceded(
            delimited(multispace1, tag("fillfactor"), multispace1),
            cut(double),
          )),
          RefposToVals::parse::<E>,
          cut(many0(preceded(multispace1, double))),
          opt(preceded(
            delimited(multispace1, tag("Redshift"), multispace1),
            cut(double),
          )),
          UnitToPixsize::parse::<E>,
        )),
      ),
      |(fillfactor, refpos_to_vals, lo_hi_limits, value, unit_to_pixsize)| Self {
        fillfactor,
        refpos_to_vals,
        lo_limits: lo_hi_limits.iter().step_by(2).copied().collect(),
        hi_limits: lo_hi_limits.into_iter().skip(1).step_by(2).collect(),
        value,
        unit_to_pixsize,
      },
    )(input)
  }
}
