use std::{
  fmt::{self, Display},
  str::FromStr,
};

use serde::{Deserialize, Serialize};

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::multispace1,
  combinator::{cut, map, opt, value},
  multi::many_m_n,
  number::complete::double,
  sequence::{preceded, tuple},
  IResult,
};

use crate::{
  common::{SpaceTimeRefPos as RefPos, ValOrRange}, space::common::velocity::VelocityUnit, NomErr,
};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum DopplerDef {
  #[serde(rename = "OPTICAL")]
  Optical,
  #[serde(rename = "RADIO")]
  Radio,
  #[serde(rename = "RELATIVISTIC")]
  Relativistic,
}
impl DopplerDef {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      value(Self::Optical, tag("OPTICAL")),
      value(Self::Radio, tag("RADIO")),
      value(Self::Relativistic, tag("RELATIVISTIC")),
    ))(input)
  }
}
impl FromStr for DopplerDef {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "OPTICAL" => Ok(Self::Optical),
      "RADIO" => Ok(Self::Radio),
      "RELATIVISTIC" => Ok(Self::Relativistic),
      _ => Err(format!(
        "Unknown DopplerDef. Actual: \"{}\"; Expected: one of [OPTICAL, RADIO, RELATIVISTIC].",
        s
      )),
    }
  }
}
impl Display for DopplerDef {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::Optical => "OPTICAL",
      Self::Radio => "RADIO",
      Self::Relativistic => "RELATIVISTIC",
    })
  }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum ZType {
  #[serde(rename = "VELOCITY")]
  Velocity,
  #[serde(rename = "REDSHIFT")]
  Redhshift,
}
impl ZType {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      value(Self::Velocity, tag("VELOCITY")),
      value(Self::Redhshift, tag("REDSHIFT")),
    ))(input)
  }
}
impl FromStr for ZType {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "VELOCITY" => Ok(Self::Velocity),
      "REDSHIFT" => Ok(Self::Redhshift),
      _ => Err(format!(
        "Unknown ztype. Actual: \"{}\"; Expected: one of [VELOCITY, REDSHIFT].",
        s
      )),
    }
  }
}
impl Display for ZType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::Velocity => "VELOCITY",
      Self::Redhshift => "REDSHIFT",
    })
  }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum VelocityUnitOrNil {
  Nil,
  VelocityUnit(VelocityUnit),
}
impl VelocityUnitOrNil {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      value(VelocityUnitOrNil::Nil, tag("nil")),
      map(VelocityUnit::parse::<E>, VelocityUnitOrNil::VelocityUnit),
    ))(input)
  }
}
impl Display for VelocityUnitOrNil {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Nil => f.write_str("nil"),
      Self::VelocityUnit(velocity_unit) => Display::fmt(velocity_unit, f),
    }
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct RefposToVals {
  #[serde(rename = "refpos", skip_serializing_if = "Option::is_none")]
  pub refpos: Option<RefPos>,
  #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
  pub ztype: Option<ZType>,
  #[serde(rename = "dopplerdef", skip_serializing_if = "Option::is_none")]
  pub doppler_def: Option<DopplerDef>,
}
impl RefposToVals {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        opt(preceded(multispace1, RefPos::parse::<E>)),
        opt(preceded(multispace1, ZType::parse::<E>)),
        opt(preceded(multispace1, DopplerDef::parse::<E>)),
      )),
      |(refpos, ztype, doppler_def)| Self {
        refpos,
        ztype,
        doppler_def,
      },
    )(input)
  }
}
impl Display for RefposToVals {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(refpos) = self.refpos.as_ref() {
      f.write_fmt(format_args!(" {}", refpos))?;
    }
    if let Some(ztype) = self.ztype.as_ref() {
      f.write_fmt(format_args!(" {}", ztype))?;
    }
    if let Some(doppler_def) = self.doppler_def.as_ref() {
      f.write_fmt(format_args!(" {}", doppler_def))?;
    }
    Ok(())
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct UnitToPixsize {
  #[serde(rename = "unit", skip_serializing_if = "Option::is_none")]
  pub unit: Option<VelocityUnitOrNil>,
  #[serde(rename = "error", skip_serializing_if = "Option::is_none")]
  pub error: Option<ValOrRange>,
  #[serde(rename = "resolution", skip_serializing_if = "Option::is_none")]
  pub resolution: Option<ValOrRange>,
  #[serde(rename = "size", skip_serializing_if = "Option::is_none")]
  pub size: Option<ValOrRange>,
  #[serde(rename = "pixsize", skip_serializing_if = "Option::is_none")]
  pub pixsize: Option<ValOrRange>,
}
impl UnitToPixsize {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        // unit
        opt(preceded(
          preceded(multispace1, tag("unit")),
          cut(preceded(multispace1, VelocityUnitOrNil::parse::<E>)),
        )),
        // error
        opt(preceded(
          preceded(multispace1, tag("Error")),
          cut(many_m_n(1, 2, preceded(multispace1, double))),
        )),
        // resolution
        opt(preceded(
          preceded(multispace1, tag("Resolution")),
          cut(many_m_n(1, 2, preceded(multispace1, double))),
        )),
        // size
        opt(preceded(
          preceded(multispace1, tag("Size")),
          cut(many_m_n(1, 2, preceded(multispace1, double))),
        )),
        // pixsize
        opt(preceded(
          preceded(multispace1, tag("PixSize")),
          cut(many_m_n(1, 2, preceded(multispace1, double))),
        )),
      )),
      |(unit, error, resolution, size, pixsize)| Self {
        unit,
        error: error.map(|v| v.into()),
        resolution: resolution.map(|v| v.into()),
        size: size.map(|v| v.into()),
        pixsize: pixsize.map(|v| v.into()),
      },
    )(input)
  }
}
impl Display for UnitToPixsize {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(unit) = self.unit.as_ref() {
      f.write_fmt(format_args!(" unit {}", unit))?;
    }
    if let Some(error) = self.error.as_ref() {
      f.write_fmt(format_args!(" Error {}", error))?;
    }
    if let Some(resolution) = self.resolution.as_ref() {
      f.write_fmt(format_args!(" Resolution {}", resolution))?;
    }
    if let Some(size) = self.size.as_ref() {
      f.write_fmt(format_args!(" Size {}", size))?;
    }
    if let Some(pixsize) = self.pixsize.as_ref() {
      f.write_fmt(format_args!(" PixSize {}", pixsize))?;
    }
    Ok(())
  }
}
