use std::fmt::{self, Display};

use nom::{
  bytes::complete::tag_no_case,
  character::complete::multispace1,
  combinator::{cut, map},
  multi::many1,
  number::complete::double,
  sequence::{preceded, tuple},
  IResult,
};
use serde::{Deserialize, Serialize};

use super::common::{
  velocity::Velocity, Flavor, Frame, FrameRefposFlavor, FromUnitToVelocity, SpaceUnit,
};
use crate::{common::SpaceTimeRefPos, NomErr};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Position {
  #[serde(flatten)]
  pub pre: FrameRefposFlavor,
  pub pos: Vec<f64>,
  #[serde(flatten)]
  pub post: FromUnitToVelocity,
}

impl Position {
  pub fn from_frame(frame: Frame) -> Self {
    Self {
      pre: FrameRefposFlavor::from_frame(frame),
      pos: Vec::with_capacity(0),
      post: FromUnitToVelocity::default(),
    }
  }

  // Setters

  pub fn set_refpos(mut self, refpos: SpaceTimeRefPos) -> Self {
    self.set_refpos_by_ref(refpos);
    self
  }
  pub fn set_flavor(mut self, flavor: Flavor) -> Self {
    self.set_flavor_by_ref(flavor);
    self
  }
  pub fn set_position(mut self, pos: Vec<f64>) -> Self {
    self.set_position_by_ref(pos);
    self
  }
  pub fn set_unit(mut self, units: Vec<SpaceUnit>) -> Self {
    self.set_unit_by_ref(units);
    self
  }
  pub fn set_error(mut self, error: Vec<f64>) -> Self {
    self.set_error_by_ref(error);
    self
  }
  pub fn set_resolution(mut self, resolution: Vec<f64>) -> Self {
    self.set_resolution_by_ref(resolution);
    self
  }
  pub fn set_size(mut self, size: Vec<f64>) -> Self {
    self.set_size_by_ref(size);
    self
  }
  pub fn set_pixsize(mut self, pixsize: Vec<f64>) -> Self {
    self.set_pixsize_by_ref(pixsize);
    self
  }
  pub fn set_velocity(mut self, velocity: Velocity) -> Self {
    self.set_velocity_by_ref(velocity);
    self
  }

  // Setters by ref

  pub fn set_refpos_by_ref(&mut self, refpos: SpaceTimeRefPos) {
    self.pre.set_refpos_by_ref(refpos);
  }
  pub fn set_flavor_by_ref(&mut self, flavor: Flavor) {
    self.pre.set_flavor_by_ref(flavor);
  }
  pub fn set_position_by_ref(&mut self, pos: Vec<f64>) {
    self.pos = pos;
  }
  pub fn set_unit_by_ref(&mut self, units: Vec<SpaceUnit>) {
    self.post.set_unit_by_ref(units);
  }
  pub fn set_error_by_ref(&mut self, error: Vec<f64>) {
    self.post.set_error_by_ref(error);
  }
  pub fn set_resolution_by_ref(&mut self, resolution: Vec<f64>) {
    self.post.set_resolution_by_ref(resolution);
  }
  pub fn set_size_by_ref(&mut self, size: Vec<f64>) {
    self.post.set_size_by_ref(size);
  }
  pub fn set_pixsize_by_ref(&mut self, pixsize: Vec<f64>) {
    self.post.set_pixsize_by_ref(pixsize);
  }
  pub fn set_velocity_by_ref(&mut self, velocity: Velocity) {
    self.post.set_velocity_by_ref(velocity);
  }

  // Getters

  pub fn frame(&self) -> Frame {
    self.pre.frame()
  }
  pub fn refpos(&self) -> Option<SpaceTimeRefPos> {
    self.pre.refpos()
  }
  pub fn refpos_or_default(&self) -> SpaceTimeRefPos {
    self.pre.refpos_or_default()
  }
  pub fn flavor(&self) -> Option<Flavor> {
    self.pre.flavor()
  }
  pub fn flavor_or_default(&self) -> Flavor {
    self.pre.flavor().unwrap_or(Flavor::Spher2)
  }
  pub fn position(&self) -> &[f64] {
    self.pos.as_slice()
  }
  pub fn unit(&self) -> Option<&Vec<SpaceUnit>> {
    self.post.unit()
  }
  pub fn error(&self) -> Option<&Vec<f64>> {
    self.post.error()
  }
  pub fn resolution(&self) -> Option<&Vec<f64>> {
    self.post.resolution()
  }
  pub fn size(&self) -> Option<&Vec<f64>> {
    self.post.size()
  }
  pub fn pixsize(&self) -> Option<&Vec<f64>> {
    self.post.pixsize()
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag_no_case("Position"),
        tuple((
          cut(FrameRefposFlavor::parse),
          // pos
          cut(many1(preceded(multispace1, double))),
          cut(FromUnitToVelocity::parse),
        )),
      ),
      |(pre, pos, post)| Self { pre, pos, post },
    )(input)
  }
}

impl Display for Position {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("Position")?;
    self.pre.fmt(f)?;
    for pos in self.pos.iter() {
      f.write_fmt(format_args!(" {}", pos))?;
    }
    self.post.fmt(f)
  }
}
