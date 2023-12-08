use std::{
  fmt::{self, Display},
  str::FromStr,
};

use serde::{Deserialize, Serialize};

use nom::{
  branch::alt,
  bytes::complete::tag_no_case,
  character::complete::multispace1,
  combinator::{cut, map, opt, value},
  multi::many_m_n,
  number::complete::double,
  sequence::{preceded, tuple},
  IResult,
};

use crate::{
  common::{SpaceTimeRefPos as RefPos, ValOrRange},
  space::common::velocity::VelocityUnit,
  NomErr,
};

#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq)]
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
      value(Self::Optical, tag_no_case("OPTICAL")),
      value(Self::Radio, tag_no_case("RADIO")),
      value(Self::Relativistic, tag_no_case("RELATIVISTIC")),
    ))(input)
  }
}
impl Default for DopplerDef {
  fn default() -> Self {
    Self::Optical
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

#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq)]
pub enum ZType {
  #[serde(rename = "VELOCITY")]
  Velocity,
  #[serde(rename = "REDSHIFT")]
  Redhshift,
}
impl ZType {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      value(Self::Velocity, tag_no_case("VELOCITY")),
      value(Self::Redhshift, tag_no_case("REDSHIFT")),
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

#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq)]
pub enum VelocityUnitOrNil {
  Nil,
  VelocityUnit(VelocityUnit),
}
impl VelocityUnitOrNil {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      value(VelocityUnitOrNil::Nil, tag_no_case("nil")),
      map(VelocityUnit::parse::<E>, VelocityUnitOrNil::VelocityUnit),
    ))(input)
  }
}
impl Default for VelocityUnitOrNil {
  fn default() -> Self {
    Self::Nil
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

#[derive(Serialize, Deserialize, Default, Debug, PartialEq)]
pub(crate) struct RefposToVals {
  #[serde(rename = "refpos", skip_serializing_if = "Option::is_none")]
  pub refpos: Option<RefPos>,
  #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
  pub ztype: Option<ZType>,
  #[serde(rename = "dopplerdef", skip_serializing_if = "Option::is_none")]
  pub doppler_def: Option<DopplerDef>,
}
impl RefposToVals {
  pub fn set_refpos_by_ref(&mut self, refpos: RefPos) {
    self.refpos.replace(refpos);
  }
  pub fn set_ztype_by_ref(&mut self, ztype: ZType) {
    self.ztype.replace(ztype);
  }
  pub fn set_doppler_def_by_ref(&mut self, doppler_def: DopplerDef) {
    self.doppler_def.replace(doppler_def);
  }

  pub fn refpos(&self) -> Option<RefPos> {
    self.refpos
  }
  pub fn refpos_or_default(&self) -> RefPos {
    self.refpos.unwrap_or_default()
  }
  pub fn ztype(&self) -> Option<ZType> {
    self.ztype
  }
  pub fn doppler_def(&self) -> Option<DopplerDef> {
    self.doppler_def
  }
  pub fn doppler_def_or_default(&self) -> DopplerDef {
    self.doppler_def.unwrap_or_default()
  }

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

#[derive(Serialize, Deserialize, Default, Debug, PartialEq)]
pub(crate) struct UnitToPixsize {
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
  // Setters by ref

  pub fn set_unit_by_ref(&mut self, unit: VelocityUnitOrNil) {
    self.unit.replace(unit);
  }
  pub fn set_error_by_ref(&mut self, error: ValOrRange) {
    self.error.replace(error);
  }
  pub fn set_resolution_by_ref(&mut self, resolution: ValOrRange) {
    self.resolution.replace(resolution);
  }
  pub fn set_size_by_ref(&mut self, size: ValOrRange) {
    self.size.replace(size);
  }
  pub fn set_pixsize_by_ref(&mut self, pixsize: ValOrRange) {
    self.pixsize.replace(pixsize);
  }

  // Getters

  pub fn unit(&self) -> Option<VelocityUnitOrNil> {
    self.unit
  }
  pub fn unit_or_default(&self) -> VelocityUnitOrNil {
    self.unit.unwrap_or_default()
  }
  pub fn error(&self) -> Option<&ValOrRange> {
    self.error.as_ref()
  }
  pub fn resolution(&self) -> Option<&ValOrRange> {
    self.resolution.as_ref()
  }
  pub fn size(&self) -> Option<&ValOrRange> {
    self.size.as_ref()
  }
  pub fn pixsize(&self) -> Option<&ValOrRange> {
    self.pixsize.as_ref()
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        // unit
        opt(preceded(
          preceded(multispace1, tag_no_case("unit")),
          cut(preceded(multispace1, VelocityUnitOrNil::parse::<E>)),
        )),
        // error
        opt(preceded(
          preceded(multispace1, tag_no_case("Error")),
          cut(many_m_n(1, 2, preceded(multispace1, double))),
        )),
        // resolution
        opt(preceded(
          preceded(multispace1, tag_no_case("Resolution")),
          cut(many_m_n(1, 2, preceded(multispace1, double))),
        )),
        // size
        opt(preceded(
          preceded(multispace1, tag_no_case("Size")),
          cut(many_m_n(1, 2, preceded(multispace1, double))),
        )),
        // pixsize
        opt(preceded(
          preceded(multispace1, tag_no_case("PixSize")),
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
