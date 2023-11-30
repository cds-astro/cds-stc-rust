use std::fmt::{self, Display};

use nom::{
  bytes::complete::tag,
  character::complete::{char, multispace1},
  combinator::{cut, map, opt},
  multi::{many1, many_m_n},
  number::complete::double,
  sequence::{delimited, preceded, separated_pair, tuple},
  IResult,
};
use serde::{Deserialize, Serialize};

use super::SpaceUnit;
use crate::{time::common::TimeUnit, NomErr};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct VelocityUnit {
  #[serde(rename = "space_unit")]
  space_unit: SpaceUnit,
  #[serde(rename = "time_unit")]
  time_unit: TimeUnit,
}
impl VelocityUnit {
  pub fn new(space_unit: SpaceUnit, time_unit: TimeUnit) -> Self {
    Self {
      space_unit,
      time_unit,
    }
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      separated_pair(SpaceUnit::parse::<E>, char('/'), TimeUnit::parse::<E>),
      |(space_unit, time_unit)| Self::new(space_unit, time_unit),
    )(input)
  }
}
impl Display for VelocityUnit {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_fmt(format_args!("{}/{}", &self.space_unit, &self.time_unit))
  }
}

/// VelocityInterval
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Velocity {
  /// Default value: 1.0.
  #[serde(skip_serializing_if = "Option::is_none")]
  fillfactor: Option<f64>,
  /*#[serde(rename = "lolimit")]
  lo_limits: Vec<f64>,
  #[serde(rename = "hilimit")]
  hi_limits: Vec<f64>,
  lo_hi_limits: Vec<Range<f64>>,
  */
  #[serde(rename = "lohilimit")]
  lo_hi_limits: Vec<f64>,
  #[serde(skip_serializing_if = "Option::is_none")]
  velocity: Option<Vec<f64>>,
  /// Default unit: 'm/s' 
  #[serde(skip_serializing_if = "Option::is_none")]
  unit: Option<Vec<VelocityUnit>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  /// Multiple errors possible.
  error: Option<Vec<f64>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  /// Multiple resolutions possible.
  resolution: Option<Vec<f64>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  /// Multiple pixsizes possible
  pixsize: Option<Vec<f64>>,
}

impl Velocity {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag("VelocityInterval"),
        tuple((
          opt(preceded(
            delimited(multispace1, tag("fillfactor"), cut(multispace1)),
            double,
          )),
          cut(many_m_n(2, usize::MAX, preceded(multispace1, double))),
          /*map(
            cut(many_m_n(2, usize::MAX, preceded(multispace1, double))),
            |v| {
              v.iter()
                .step_by(2)
                .zip(v.iter().skip(1).step_by(2))
                .map(|(l, r)| (*l, *r))
                .unzip()
            },
          ),*/
          opt(preceded(
            preceded(multispace1, tag("Velocity")),
            cut(many_m_n(1, usize::MAX, preceded(multispace1, double))),
          )),
          opt(preceded(
            preceded(multispace1, tag("unit")),
            cut(many1(preceded(multispace1, VelocityUnit::parse::<E>))),
          )),
          opt(preceded(
            preceded(multispace1, tag("Error")),
            cut(many1(preceded(multispace1, double))),
          )),
          opt(preceded(
            preceded(multispace1, tag("Resolution")),
            cut(many1(preceded(multispace1, double))),
          )),
          opt(preceded(
            preceded(multispace1, tag("PixSize")),
            cut(many1(preceded(multispace1, double))),
          )),
        )),
      ),
      |(fillfactor, lo_hi_limits, velocity, unit, error, resolution, pixsize)| Self {
        fillfactor,
        // lo_limits: lo_hi_limits.0,
        // hi_limits: lo_hi_limits.1,
        lo_hi_limits,
        velocity,
        unit,
        error,
        resolution,
        pixsize,
      },
    )(input)
  }
}

impl Display for Velocity {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("VelocityInterval")?;
    if let Some(fillfactor) = self.fillfactor {
      f.write_fmt(format_args!(" fillfactor {}", fillfactor))?;
    }
    /*for (start, end) in self.lo_limits.iter().zip(self.hi_limits.iter()) {
      f.write_fmt(format_args!(" {} {}", start, end))?;
    }*/
    for v in self.lo_hi_limits.iter() {
      f.write_fmt(format_args!(" {}", v))?;
    }
    if let Some(velocities) = self.velocity.as_ref() {
      f.write_str(" Velocity")?;
      for v in velocities {
        f.write_fmt(format_args!(" {}", v))?;
      }
    }
    if let Some(unit) = self.unit.as_ref() {
      f.write_str(" unit")?;
      for u in unit {
        f.write_fmt(format_args!(" {}", u))?;
      }
    }
    if let Some(errors) = self.error.as_ref() {
      f.write_str(" Error")?;
      for e in errors {
        f.write_fmt(format_args!(" {}", e))?;
      }
    }
    if let Some(resolutions) = self.resolution.as_ref() {
      f.write_str(" Resolution")?;
      for r in resolutions {
        f.write_fmt(format_args!(" {}", r))?;
      }
    }
    if let Some(pixsizes) = self.pixsize.as_ref() {
      f.write_str(" PixSize")?;
      for p in pixsizes {
        f.write_fmt(format_args!(" {}", p))?;
      }
    }
    Ok(())
  }
}
