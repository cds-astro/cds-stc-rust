use std::fmt::{self, Display};

use serde::{Deserialize, Serialize};

use nom::{
  branch::alt,
  bytes::complete::tag_no_case,
  character::complete::multispace1,
  combinator::{cut, map, opt},
  multi::many0,
  number::complete::double,
  sequence::{delimited, preceded, tuple},
  IResult,
};

use super::NomErr;

use crate::{
  common::{SpaceTimeRefPos as RefPos, ValOrRange},
  visitor::SpectralVisitor,
};

pub mod common;
use common::*;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum Spectral {
  Value(SpecValue),
  Interval(SpecInterval),
}
impl Spectral {
  pub fn accept<V: SpectralVisitor>(&self, visitor: V) -> Result<V::Value, V::Error> {
    match self {
      Self::Value(value) => visitor.visit_spectral_simple(value),
      Self::Interval(interval) => visitor.visit_spectral_interval(interval),
    }
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      map(SpecInterval::parse::<E>, Self::Interval),
      map(SpecValue::parse::<E>, Self::Value),
    ))(input)
  }
}
impl Display for Spectral {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Value(e) => Display::fmt(e, f),
      Self::Interval(e) => Display::fmt(e, f),
    }
  }
}
impl From<SpecValue> for Spectral {
  fn from(value: SpecValue) -> Self {
    Self::Value(value)
  }
}
impl From<SpecInterval> for Spectral {
  fn from(value: SpecInterval) -> Self {
    Self::Interval(value)
  }
}

#[derive(Serialize, Deserialize, Default, Debug, PartialEq)]
pub struct SpecValue {
  refpos: Option<RefPos>,
  #[serde(skip_serializing_if = "Option::is_none")]
  value: Option<f64>,
  #[serde(flatten)]
  unit_to_pixsize: UnitToPixsize,
}
impl SpecValue {
  pub fn new() -> Self {
    Self::default()
  }

  // Setters

  pub fn set_refpos(mut self, refpos: RefPos) -> Self {
    self.set_refpos_by_ref(refpos);
    self
  }
  pub fn set_value(mut self, value: f64) -> Self {
    self.set_value_by_ref(value);
    self
  }
  pub fn set_unit(mut self, unit: SpectralUnit) -> Self {
    self.set_unit_by_ref(unit);
    self
  }
  pub fn set_error(mut self, error: ValOrRange) -> Self {
    self.set_error_by_ref(error);
    self
  }
  pub fn set_resolution(mut self, resolution: ValOrRange) -> Self {
    self.set_resolution_by_ref(resolution);
    self
  }
  pub fn set_size(mut self, size: ValOrRange) -> Self {
    self.set_size_by_ref(size);
    self
  }
  pub fn set_pixsize(mut self, pixsize: ValOrRange) -> Self {
    self.set_pixsize_by_ref(pixsize);
    self
  }

  // Setters by ref

  pub fn set_refpos_by_ref(&mut self, refpos: RefPos) {
    self.refpos.replace(refpos);
  }
  pub fn set_value_by_ref(&mut self, value: f64) {
    self.value.replace(value);
  }
  pub fn set_unit_by_ref(&mut self, unit: SpectralUnit) {
    self.unit_to_pixsize.set_unit_by_ref(unit);
  }
  pub fn set_error_by_ref(&mut self, error: ValOrRange) {
    self.unit_to_pixsize.set_error_by_ref(error);
  }
  pub fn set_resolution_by_ref(&mut self, resolution: ValOrRange) {
    self.unit_to_pixsize.set_resolution_by_ref(resolution);
  }
  pub fn set_size_by_ref(&mut self, size: ValOrRange) {
    self.unit_to_pixsize.set_size_by_ref(size);
  }
  pub fn set_pixsize_by_ref(&mut self, pixsize: ValOrRange) {
    self.unit_to_pixsize.set_pixsize_by_ref(pixsize);
  }

  // Getters

  pub fn refpos(&self) -> Option<RefPos> {
    self.refpos
  }
  pub fn refpos_or_default(&self) -> RefPos {
    self.refpos.unwrap_or_default()
  }
  pub fn value(&self) -> Option<f64> {
    self.value
  }
  pub fn unit(&self) -> Option<SpectralUnit> {
    self.unit_to_pixsize.unit()
  }
  pub fn unit_or_default(&self) -> SpectralUnit {
    self.unit_to_pixsize.unit_or_default()
  }
  pub fn error(&self) -> Option<&ValOrRange> {
    self.unit_to_pixsize.error()
  }
  pub fn resolution(&self) -> Option<&ValOrRange> {
    self.unit_to_pixsize.resolution()
  }
  pub fn size(&self) -> Option<&ValOrRange> {
    self.unit_to_pixsize.size()
  }
  pub fn pixsize(&self) -> Option<&ValOrRange> {
    self.unit_to_pixsize.pixsize()
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag_no_case("Spectral"),
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
  pub fn new(lo_limits: Vec<f64>, hi_limits: Vec<f64>) -> Result<Self, String> {
    if lo_limits.len() == hi_limits.len() {
      Err(format!(
        "Error in RedshiftInterval::new(). Lo and Hi limits have different length: {} != {}",
        lo_limits.len(),
        hi_limits.len()
      ))
    } else {
      Ok(Self {
        fillfactor: None,
        refpos: None,
        lo_limits,
        hi_limits,
        spectral: None,
        unit_to_pixsize: UnitToPixsize::default(),
      })
    }
  }

  // Setters

  pub fn set_fillfactor(mut self, fillfactor: f64) -> Self {
    self.set_fillfactor_by_ref(fillfactor);
    self
  }
  pub fn set_refpos(mut self, refpos: RefPos) -> Self {
    self.set_refpos_by_ref(refpos);
    self
  }
  pub fn set_spectral(mut self, spectral: f64) -> Self {
    self.set_spectral_by_ref(spectral);
    self
  }
  pub fn set_unit(mut self, unit: SpectralUnit) -> Self {
    self.set_unit_by_ref(unit);
    self
  }
  pub fn set_error(mut self, error: ValOrRange) -> Self {
    self.set_error_by_ref(error);
    self
  }
  pub fn set_resolution(mut self, resolution: ValOrRange) -> Self {
    self.set_resolution_by_ref(resolution);
    self
  }
  pub fn set_size(mut self, size: ValOrRange) -> Self {
    self.set_size_by_ref(size);
    self
  }
  pub fn set_pixsize(mut self, pixsize: ValOrRange) -> Self {
    self.set_pixsize_by_ref(pixsize);
    self
  }

  // Setters by ref

  pub fn set_fillfactor_by_ref(&mut self, fillfactor: f64) {
    self.fillfactor.replace(fillfactor);
  }
  pub fn set_refpos_by_ref(&mut self, refpos: RefPos) {
    self.refpos.replace(refpos);
  }
  pub fn set_spectral_by_ref(&mut self, spectral: f64) {
    self.spectral.replace(spectral);
  }
  pub fn set_unit_by_ref(&mut self, unit: SpectralUnit) {
    self.unit_to_pixsize.set_unit_by_ref(unit);
  }
  pub fn set_error_by_ref(&mut self, error: ValOrRange) {
    self.unit_to_pixsize.set_error_by_ref(error);
  }
  pub fn set_resolution_by_ref(&mut self, resolution: ValOrRange) {
    self.unit_to_pixsize.set_resolution_by_ref(resolution);
  }
  pub fn set_size_by_ref(&mut self, size: ValOrRange) {
    self.unit_to_pixsize.set_size_by_ref(size);
  }
  pub fn set_pixsize_by_ref(&mut self, pixsize: ValOrRange) {
    self.unit_to_pixsize.set_pixsize_by_ref(pixsize);
  }

  // Getters

  pub fn fillfactor(&self) -> Option<f64> {
    self.fillfactor
  }
  pub fn fillfactor_or_default(&self) -> f64 {
    self.fillfactor().unwrap_or(1.0)
  }
  pub fn refpos(&self) -> Option<RefPos> {
    self.refpos
  }
  pub fn refpos_or_default(&self) -> RefPos {
    self.refpos.unwrap_or_default()
  }
  pub fn lo_limits(&self) -> &[f64] {
    &self.lo_limits
  }
  pub fn hi_limits(&self) -> &[f64] {
    &self.hi_limits
  }
  pub fn spectral(&self) -> Option<f64> {
    self.spectral
  }
  pub fn unit(&self) -> Option<SpectralUnit> {
    self.unit_to_pixsize.unit()
  }
  pub fn unit_or_default(&self) -> SpectralUnit {
    self.unit_to_pixsize.unit_or_default()
  }
  pub fn error(&self) -> Option<&ValOrRange> {
    self.unit_to_pixsize.error()
  }
  pub fn resolution(&self) -> Option<&ValOrRange> {
    self.unit_to_pixsize.resolution()
  }
  pub fn size(&self) -> Option<&ValOrRange> {
    self.unit_to_pixsize.size()
  }
  pub fn pixsize(&self) -> Option<&ValOrRange> {
    self.unit_to_pixsize.pixsize()
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag_no_case("SpectralInterval"),
        tuple((
          opt(preceded(
            delimited(multispace1, tag_no_case("fillfactor"), multispace1),
            cut(double),
          )),
          opt(preceded(multispace1, RefPos::parse::<E>)),
          cut(many0(preceded(multispace1, double))),
          opt(preceded(
            delimited(multispace1, tag_no_case("Spectral"), multispace1),
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
