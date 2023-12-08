use std::fmt::{self, Display};

use nom::{
  bytes::complete::tag_no_case,
  character::complete::multispace1,
  combinator::{cut, map},
  multi::many_m_n,
  number::complete::double,
  sequence::{preceded, tuple},
  IResult,
};
use serde::{Deserialize, Serialize};

use super::common::{
  velocity::Velocity, FillFrameRefposFlavor, Flavor, Frame, FromPosToVelocity, SpaceUnit,
};
use crate::{common::SpaceTimeRefPos, NomErr};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct PositionInterval {
  #[serde(flatten)]
  pub pre: FillFrameRefposFlavor,
  // TODO: We so far keep a single vector because we are not sure of how coordinates are grouped...
  pub lo_hi_limits: Vec<f64>, // Vec<Range<f64>>
  #[serde(flatten)]
  pub post: FromPosToVelocity,
}
impl PositionInterval {
  pub fn from_frame(frame: Frame) -> Self {
    Self {
      pre: FillFrameRefposFlavor::from_frame(frame),
      lo_hi_limits: Vec::with_capacity(0),
      post: FromPosToVelocity::default(),
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
  pub fn set_flavor(mut self, flavor: Flavor) -> Self {
    self.set_flavor_by_ref(flavor);
    self
  }
  pub fn set_lo_hi_limits(mut self, pos: Vec<f64>) -> Self {
    self.set_lo_hi_limits_by_ref(pos);
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

  pub fn set_fillfactor_by_ref(&mut self, fillfactor: f64) {
    self.pre.set_fillfactor_by_ref(fillfactor);
  }
  pub fn set_refpos_by_ref(&mut self, refpos: SpaceTimeRefPos) {
    self.pre.set_refpos_by_ref(refpos);
  }
  pub fn set_flavor_by_ref(&mut self, flavor: Flavor) {
    self.pre.set_flavor_by_ref(flavor);
  }
  pub fn set_lo_hi_limits_by_ref(&mut self, lo_hi_limits: Vec<f64>) {
    self.lo_hi_limits = lo_hi_limits;
  }
  pub fn set_position_by_ref(&mut self, pos: Vec<f64>) {
    self.post.set_position_by_ref(pos);
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

  pub fn fillfactor(&self) -> Option<f64> {
    self.pre.fillfactor()
  }
  pub fn fillfactor_or_default(&self) -> f64 {
    self.pre.fillfactor_or_default()
  }
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
  pub fn lo_hi_limits(&self) -> &[f64] {
    self.lo_hi_limits.as_slice()
  }
  pub fn position(&self) -> Option<&Vec<f64>> {
    self.post.position()
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
        tag_no_case("PositionInterval"),
        tuple((
          cut(FillFrameRefposFlavor::parse),
          // pos and radius
          cut(many_m_n(2, usize::MAX, preceded(multispace1, double))),
          cut(FromPosToVelocity::parse),
        )),
      ),
      |(pre, lo_hi_limits, post)| Self {
        pre,
        lo_hi_limits,
        post,
      },
    )(input)
  }
}
impl Display for PositionInterval {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("PositionInterval")?;
    self.pre.fmt(f)?;
    for elem in self.lo_hi_limits.iter() {
      f.write_fmt(format_args!(" {}", elem))?;
    }
    self.post.fmt(f)
  }
}

#[cfg(test)]
mod tests {
  use nom::error::VerboseError;

  use super::{super::common::Frame, PositionInterval};
  use crate::common::SpaceTimeRefPos as RefPos;

  #[test]
  fn test_position_interval() {
    let interval = PositionInterval::from_frame(Frame::ICRS)
      .set_refpos(RefPos::Geocenter)
      .set_lo_hi_limits(vec![170.0, -20.0, 190.0, 10.0])
      .set_resolution(vec![0.0001]);
    let s = "PositionInterval ICRS GEOCENTER 170 -20 190 10 Resolution 0.0001";
    let (rem, pi) = PositionInterval::parse::<VerboseError<&str>>(s).unwrap();
    assert_eq!(interval, pi, "{}", rem);
    assert_eq!(interval.to_string().as_str(), s);

    let json = serde_json::to_string_pretty(&interval).unwrap();
    // println!("{}", json);
    assert_eq!(
      json,
      r#"{
  "frame": "ICRS",
  "refpos": "GEOCENTER",
  "lo_hi_limits": [
    170.0,
    -20.0,
    190.0,
    10.0
  ],
  "resolution": [
    0.0001
  ]
}"#
    );
  }
}
