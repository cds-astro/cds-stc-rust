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

use super::{
  common::{SpaceTimeRefPos, ValOrRange},
  visitor::RedshiftVisitor,
  NomErr,
};

pub mod common;
use common::*;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum Redshift {
  #[serde(rename = "Redshift")]
  Value(RedshiftValue),
  #[serde(rename = "RedshiftInterval")]
  Interval(RedshiftInterval),
}
impl Redshift {
  pub fn accept<V: RedshiftVisitor>(&self, visitor: V) -> Result<V::Value, V::Error> {
    match self {
      Self::Value(z) => visitor.visit_redshift_simple(z),
      Self::Interval(z) => visitor.visit_redshift_interval(z),
    }
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      map(RedshiftInterval::parse::<E>, Self::Interval),
      map(RedshiftValue::parse::<E>, Self::Value),
    ))(input)
  }
}
impl Display for Redshift {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Value(e) => Display::fmt(e, f),
      Self::Interval(e) => Display::fmt(e, f),
    }
  }
}
impl From<RedshiftValue> for Redshift {
  fn from(value: RedshiftValue) -> Self {
    Self::Value(value)
  }
}
impl From<RedshiftInterval> for Redshift {
  fn from(value: RedshiftInterval) -> Self {
    Self::Interval(value)
  }
}

#[derive(Serialize, Deserialize, Default, Debug, PartialEq)]
pub struct RedshiftValue {
  #[serde(flatten)]
  refpos_to_vals: RefposToVals,
  #[serde(rename = "redshift", skip_serializing_if = "Option::is_none")]
  value: Option<f64>,
  #[serde(flatten)]
  unit_to_pixsize: UnitToPixsize,
}
impl RedshiftValue {
  pub fn new() -> Self {
    Self::default()
  }

  // Setters

  pub fn set_refpos(mut self, refpos: SpaceTimeRefPos) -> Self {
    self.set_refpos_by_ref(refpos);
    self
  }
  pub fn set_ztype(mut self, ztype: ZType) -> Self {
    self.set_ztype_by_ref(ztype);
    self
  }
  pub fn set_doppler_def(mut self, doppler_def: DopplerDef) -> Self {
    self.set_doppler_def_by_ref(doppler_def);
    self
  }
  pub fn set_value(mut self, z: f64) -> Self {
    self.set_value_by_ref(z);
    self
  }
  pub fn set_unit(mut self, unit: VelocityUnitOrNil) -> Self {
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

  pub fn set_refpos_by_ref(&mut self, refpos: SpaceTimeRefPos) {
    self.refpos_to_vals.set_refpos_by_ref(refpos)
  }
  pub fn set_ztype_by_ref(&mut self, ztype: ZType) {
    self.refpos_to_vals.set_ztype_by_ref(ztype)
  }
  pub fn set_doppler_def_by_ref(&mut self, doppler_def: DopplerDef) {
    self.refpos_to_vals.set_doppler_def_by_ref(doppler_def)
  }
  pub fn set_value_by_ref(&mut self, z: f64) {
    self.value.replace(z);
  }
  pub fn set_unit_by_ref(&mut self, unit: VelocityUnitOrNil) {
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

  pub fn refpos(&self) -> Option<SpaceTimeRefPos> {
    self.refpos_to_vals.refpos()
  }
  pub fn refpos_or_default(&self) -> SpaceTimeRefPos {
    self.refpos_to_vals.refpos_or_default()
  }
  pub fn ztype(&self) -> Option<ZType> {
    self.refpos_to_vals.ztype()
  }
  pub fn doppler_def(&self) -> Option<DopplerDef> {
    self.refpos_to_vals.doppler_def()
  }
  pub fn doppler_def_or_default(&self) -> DopplerDef {
    self.refpos_to_vals.doppler_def_or_default()
  }
  pub fn value(&self) -> Option<f64> {
    self.value
  }
  pub fn unit(&self) -> Option<VelocityUnitOrNil> {
    self.unit_to_pixsize.unit()
  }
  pub fn unit_or_default(&self) -> VelocityUnitOrNil {
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
        tag_no_case("Redshift"),
        tuple((
          RefposToVals::parse::<E>,
          opt(preceded(multispace1, double)),
          UnitToPixsize::parse::<E>,
        )),
      ),
      |(refpos_to_vals, values, unit_to_pixsize)| Self {
        refpos_to_vals,
        value: values,
        unit_to_pixsize,
      },
    )(input)
  }
}
impl Display for RedshiftValue {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("Redshift")?;
    Display::fmt(&self.refpos_to_vals, f)?;
    for v in self.value.iter() {
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
        refpos_to_vals: RefposToVals::default(),
        lo_limits,
        hi_limits,
        value: None,
        unit_to_pixsize: UnitToPixsize::default(),
      })
    }
  }

  // Setters

  pub fn set_fillfactor(mut self, fillfactor: f64) -> Self {
    self.set_fillfactor_by_ref(fillfactor);
    self
  }
  pub fn set_refpos(mut self, refpos: SpaceTimeRefPos) -> Self {
    self.set_refpos_by_ref(refpos);
    self
  }
  pub fn set_ztype(mut self, ztype: ZType) -> Self {
    self.set_ztype_by_ref(ztype);
    self
  }
  pub fn set_doppler_def(mut self, doppler_def: DopplerDef) -> Self {
    self.set_doppler_def_by_ref(doppler_def);
    self
  }
  pub fn set_value(mut self, z: f64) -> Self {
    self.set_value_by_ref(z);
    self
  }
  pub fn set_unit(mut self, unit: VelocityUnitOrNil) -> Self {
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
  pub fn set_refpos_by_ref(&mut self, refpos: SpaceTimeRefPos) {
    self.refpos_to_vals.set_refpos_by_ref(refpos)
  }
  pub fn set_ztype_by_ref(&mut self, ztype: ZType) {
    self.refpos_to_vals.set_ztype_by_ref(ztype)
  }
  pub fn set_doppler_def_by_ref(&mut self, doppler_def: DopplerDef) {
    self.refpos_to_vals.set_doppler_def_by_ref(doppler_def)
  }
  pub fn set_value_by_ref(&mut self, z: f64) {
    self.value.replace(z);
  }
  pub fn set_unit_by_ref(&mut self, unit: VelocityUnitOrNil) {
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
  pub fn refpos(&self) -> Option<SpaceTimeRefPos> {
    self.refpos_to_vals.refpos()
  }
  pub fn refpos_or_default(&self) -> SpaceTimeRefPos {
    self.refpos_to_vals.refpos_or_default()
  }
  pub fn ztype(&self) -> Option<ZType> {
    self.refpos_to_vals.ztype()
  }
  pub fn doppler_def(&self) -> Option<DopplerDef> {
    self.refpos_to_vals.doppler_def()
  }
  pub fn doppler_def_or_default(&self) -> DopplerDef {
    self.refpos_to_vals.doppler_def_or_default()
  }
  pub fn lo_limits(&self) -> &[f64] {
    &self.lo_limits
  }
  pub fn hi_limits(&self) -> &[f64] {
    &self.hi_limits
  }
  pub fn value(&self) -> Option<f64> {
    self.value
  }
  pub fn unit(&self) -> Option<VelocityUnitOrNil> {
    self.unit_to_pixsize.unit()
  }
  pub fn unit_or_default(&self) -> VelocityUnitOrNil {
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
        tag_no_case("RedshiftInterval"),
        tuple((
          opt(preceded(
            delimited(multispace1, tag_no_case("fillfactor"), multispace1),
            cut(double),
          )),
          RefposToVals::parse::<E>,
          cut(many0(preceded(multispace1, double))),
          opt(preceded(
            delimited(multispace1, tag_no_case("Redshift"), multispace1),
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
