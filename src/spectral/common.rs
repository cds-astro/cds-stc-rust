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

use crate::{common::ValOrRange, NomErr};

#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq)]
pub enum SpectralUnit {
  #[serde(rename = "Angstrom")]
  Angstrom,
  #[serde(rename = "GHz")]
  GHz,
  #[serde(rename = "MeV")]
  MeV,
  #[serde(rename = "MHz")]
  MHz,
  #[serde(rename = "keV")]
  KeV,
  #[serde(rename = "Hz")]
  Hz,
  #[serde(rename = "nm")]
  Nm,
  #[serde(rename = "mm")]
  Mm,
  #[serde(rename = "um")]
  Um,
  #[serde(rename = "eV")]
  EV,
  #[serde(rename = "m")]
  M,
}
impl SpectralUnit {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      value(Self::Angstrom, tag_no_case("Angstrom")),
      value(Self::GHz, tag_no_case("GHz")),
      value(Self::MeV, tag_no_case("MeV")),
      value(Self::MHz, tag_no_case("MHz")),
      value(Self::KeV, tag_no_case("keV")),
      value(Self::Hz, tag_no_case("Hz")),
      value(Self::Nm, tag_no_case("nm")),
      value(Self::Mm, tag_no_case("mm")),
      value(Self::Um, tag_no_case("um")),
      value(Self::EV, tag_no_case("eV")),
      value(Self::M, tag_no_case("m")),
    ))(input)
  }
}
impl Default for SpectralUnit {
  fn default() -> Self {
    Self::Hz
  }
}
impl FromStr for SpectralUnit {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "Angstrom" => Ok(Self::Angstrom),
      "GHz" => Ok(Self::GHz),
      "MeV" => Ok(Self::MeV),
      "MHz" => Ok(Self::MHz),
      "keV" => Ok(Self::KeV),
      "Hz" => Ok(Self::Hz),
      "nm" => Ok(Self::Nm),
      "mm" => Ok(Self::Mm),
      "um" => Ok(Self::Um),
      "eV" => Ok(Self::EV),
      "m" => Ok(Self::M),
      _ => Err(format!(
        "Unknown SpectralUnit. Actual: \"{}\"; Expected: one of [Angstrom, GHz, MeV, MHz, keV, Hz, nm, mm, um, eV, m].",
        s
      )),
    }
  }
}
impl Display for SpectralUnit {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::Angstrom => "Angstrom",
      Self::GHz => "GHz",
      Self::MeV => "MeV",
      Self::MHz => "MHz",
      Self::KeV => "keV",
      Self::Hz => "Hz",
      Self::Nm => "nm",
      Self::Mm => "mm",
      Self::Um => "um",
      Self::EV => "eV",
      Self::M => "m",
    })
  }
}

#[derive(Serialize, Deserialize, Default, Debug, PartialEq)]
pub(crate) struct UnitToPixsize {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub unit: Option<SpectralUnit>,
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

  pub fn set_unit_by_ref(&mut self, unit: SpectralUnit) {
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

  pub fn unit(&self) -> Option<SpectralUnit> {
    self.unit
  }
  pub fn unit_or_default(&self) -> SpectralUnit {
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
          cut(preceded(multispace1, SpectralUnit::parse::<E>)),
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
