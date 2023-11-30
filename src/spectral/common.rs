use std::{
  fmt::{self, Display},
  str::FromStr,
};

use serde::{Deserialize, Serialize};

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{char, multispace1},
  combinator::{cut, map, opt, value},
  multi::many_m_n,
  number::complete::double,
  sequence::{preceded, tuple},
  IResult,
};

use crate::{NomErr, common::ValOrRange};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
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
      value(Self::Angstrom, tag("Angstrom")),
      value(Self::GHz, tag("GHz")),
      value(Self::MeV, tag("MeV")),
      value(Self::MHz, tag("MHz")),
      value(Self::KeV, tag("keV")),
      value(Self::Hz, tag("Hz")),
      value(Self::Nm, tag("nm")),
      value(Self::Mm, tag("mm")),
      value(Self::Um, tag("um")),
      value(Self::EV, tag("eV")),
      value(Self::M, char('m')),
    ))(input)
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

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct UnitToPixsize {
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
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        // unit
        opt(preceded(
          preceded(multispace1, tag("unit")),
          cut(preceded(multispace1, SpectralUnit::parse::<E>)),
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
