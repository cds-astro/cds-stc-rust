use std::fmt::{self, Debug, Display};

use nom::{
  branch::alt,
  bytes::complete::tag_no_case,
  combinator::{cut, map},
  sequence::{preceded, tuple},
  IResult,
};
use serde::{Deserialize, Serialize};

use super::common::{
  region::{
    AllSkyParams, BoxParams, CircleParams, ConvexParams, EllipseParams, PolygonParams, RegionParams,
  },
  velocity::Velocity,
  FillFrameRefposFlavor, Flavor, Frame, FromPosToVelocity, SpaceUnit,
};
use crate::{
  common::SpaceTimeRefPos,
  visitor::{CompoundVisitor, SpaceVisitor},
  NomErr,
};

/// Enumeration for all possible `Geometry`.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum GeometryEnum {
  AllSky(Geometry<AllSkyParams>),
  Circle(Geometry<CircleParams>),
  Ellipse(Geometry<EllipseParams>),
  Box(Geometry<BoxParams>),
  Polygon(Geometry<PolygonParams>),
  Convex(Geometry<ConvexParams>),
}
impl GeometryEnum {
  pub fn accept<V: SpaceVisitor>(&self, visitor: V) -> Result<V::Value, V::Error> {
    match self {
      Self::AllSky(geom) => geom.accept(visitor),
      Self::Circle(geom) => geom.accept(visitor),
      Self::Ellipse(geom) => geom.accept(visitor),
      Self::Box(geom) => geom.accept(visitor),
      Self::Polygon(geom) => geom.accept(visitor),
      Self::Convex(geom) => geom.accept(visitor),
    }
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      map(Geometry::<AllSkyParams>::parse, Self::AllSky),
      map(Geometry::<CircleParams>::parse, Self::Circle),
      map(Geometry::<EllipseParams>::parse, Self::Ellipse),
      map(Geometry::<BoxParams>::parse, Self::Box),
      map(Geometry::<PolygonParams>::parse, Self::Polygon),
      map(Geometry::<ConvexParams>::parse, Self::Convex),
    ))(input)
  }
}
impl Display for GeometryEnum {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::AllSky(geom) => Display::fmt(geom, f),
      Self::Circle(geom) => Display::fmt(geom, f),
      Self::Ellipse(geom) => Display::fmt(geom, f),
      Self::Box(geom) => Display::fmt(geom, f),
      Self::Polygon(geom) => Display::fmt(geom, f),
      Self::Convex(geom) => Display::fmt(geom, f),
    }
  }
}
impl From<Geometry<AllSkyParams>> for GeometryEnum {
  fn from(value: Geometry<AllSkyParams>) -> Self {
    Self::AllSky(value)
  }
}
impl From<Geometry<CircleParams>> for GeometryEnum {
  fn from(value: Geometry<CircleParams>) -> Self {
    Self::Circle(value)
  }
}
impl From<Geometry<EllipseParams>> for GeometryEnum {
  fn from(value: Geometry<EllipseParams>) -> Self {
    Self::Ellipse(value)
  }
}
impl From<Geometry<BoxParams>> for GeometryEnum {
  fn from(value: Geometry<BoxParams>) -> Self {
    Self::Box(value)
  }
}
impl From<Geometry<PolygonParams>> for GeometryEnum {
  fn from(value: Geometry<PolygonParams>) -> Self {
    Self::Polygon(value)
  }
}
impl From<Geometry<ConvexParams>> for GeometryEnum {
  fn from(value: Geometry<ConvexParams>) -> Self {
    Self::Convex(value)
  }
}

/// A generic region, defined from its parameters type.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Geometry<T: RegionParams> {
  #[serde(flatten)]
  pub pre: FillFrameRefposFlavor,
  #[serde(flatten)]
  pub params: T,
  #[serde(flatten)]
  pub post: FromPosToVelocity,
}
impl<T: RegionParams> Geometry<T> {
  pub fn new(pre: FillFrameRefposFlavor, params: T, post: FromPosToVelocity) -> Self {
    Self { pre, params, post }
  }
  pub fn from_frame_and_params(frame: Frame, params: T) -> Self {
    Self::new(
      FillFrameRefposFlavor::from_frame(frame),
      params,
      FromPosToVelocity::default(),
    )
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
  pub fn params(&self) -> &T {
    &self.params
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

  fn accept_gen<V: SpaceVisitor>(
    &self,
    visitor: &mut V,
  ) -> Result<<V::C as CompoundVisitor>::Value, V::Error> {
    visitor
      .new_compound_visitor(&self.pre, &self.post)
      .and_then(|mut visitor| self.params.accept(&mut visitor))
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag_no_case(T::REGION_NAME),
        tuple((
          cut(FillFrameRefposFlavor::parse),
          cut(T::parse),
          cut(FromPosToVelocity::parse),
        )),
      ),
      |(pre, params, post)| Self { pre, params, post },
    )(input)
  }
}
impl<T: RegionParams> Display for Geometry<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(T::REGION_NAME)?;
    Display::fmt(&self.pre, f)?;
    Display::fmt(&self.params, f)?;
    Display::fmt(&self.post, f)
  }
}

impl Geometry<AllSkyParams> {
  pub fn new_circle(frame: Frame) -> Self {
    Self::from_frame_and_params(frame, AllSkyParams)
  }
  pub fn flavor_or_default(&self) -> Flavor {
    self.pre.flavor().unwrap_or(Flavor::Spher2)
  }
  pub fn accept<V: SpaceVisitor>(&self, mut visitor: V) -> Result<V::Value, V::Error> {
    self
      .accept_gen(&mut visitor)
      .and_then(|res| visitor.visit_allsky(res))
  }
}
impl Geometry<CircleParams> {
  pub fn new_circle(frame: Frame, center: Vec<f64>, radius: f64) -> Self {
    Self::from_frame_and_params(frame, CircleParams::new(center, radius))
  }
  pub fn center(&self) -> &[f64] {
    self.params.center()
  }
  pub fn radius(&self) -> f64 {
    self.params.radius()
  }
  pub fn flavor_or_default(&self) -> Flavor {
    self.pre.flavor().unwrap_or(Flavor::Spher2)
  }
  pub fn accept<V: SpaceVisitor>(&self, mut visitor: V) -> Result<V::Value, V::Error> {
    self
      .accept_gen(&mut visitor)
      .and_then(|res| visitor.visit_circle(res))
  }
}
impl Geometry<EllipseParams> {
  pub fn new_ellipse(
    frame: Frame,
    center: Vec<f64>,
    radius_a: f64,
    radius_b: f64,
    pos_angle: f64,
  ) -> Self {
    Self::from_frame_and_params(
      frame,
      EllipseParams::new(center, radius_a, radius_b, pos_angle),
    )
  }
  pub fn center(&self) -> &[f64] {
    self.params.center()
  }
  pub fn radius_a(&self) -> f64 {
    self.params.radius_a()
  }
  pub fn radius_b(&self) -> f64 {
    self.params.radius_b()
  }
  pub fn pos_angle(&self) -> f64 {
    self.params.pos_angle()
  }
  pub fn flavor_or_default(&self) -> Flavor {
    self.pre.flavor().unwrap_or(Flavor::Spher2)
  }
  pub fn accept<V: SpaceVisitor>(&self, mut visitor: V) -> Result<V::Value, V::Error> {
    self
      .accept_gen(&mut visitor)
      .and_then(|res| visitor.visit_ellipse(res))
  }
}

impl Geometry<BoxParams> {
  pub fn new_box(frame: Frame, center: Vec<f64>, bsize: Vec<f64>) -> Self {
    Self::from_frame_and_params(frame, BoxParams::new(center, bsize))
  }
  pub fn center(&self) -> &[f64] {
    self.params.center()
  }
  pub fn bsize(&self) -> &[f64] {
    self.params.bsize()
  }
  pub fn flavor_or_default(&self) -> Flavor {
    self.pre.flavor().unwrap_or(Flavor::Spher2)
  }
  pub fn accept<V: SpaceVisitor>(&self, mut visitor: V) -> Result<V::Value, V::Error> {
    self
      .accept_gen(&mut visitor)
      .and_then(|res| visitor.visit_box(res))
  }
}

impl Geometry<PolygonParams> {
  pub fn new_polygon(frame: Frame, vertices: Vec<f64>) -> Self {
    Self::from_frame_and_params(frame, PolygonParams::new(vertices))
  }
  pub fn vertices(&self) -> &[f64] {
    self.params.vertices()
  }
  pub fn flavor_or_default(&self) -> Flavor {
    self.pre.flavor().unwrap_or(Flavor::Spher2)
  }
  pub fn accept<V: SpaceVisitor>(&self, mut visitor: V) -> Result<V::Value, V::Error> {
    self
      .accept_gen(&mut visitor)
      .and_then(|res| visitor.visit_polygon(res))
  }
}

impl Geometry<ConvexParams> {
  pub fn new_convex(frame: Frame, hspace: Vec<f64>) -> Self {
    Self::from_frame_and_params(frame, ConvexParams::new(hspace))
  }
  pub fn hspace(&self) -> &[f64] {
    self.params.hspace()
  }
  pub fn flavor_or_default(&self) -> Flavor {
    self.pre.flavor().unwrap_or(Flavor::UnitSpher)
  }
  pub fn accept<V: SpaceVisitor>(&self, mut visitor: V) -> Result<V::Value, V::Error> {
    self
      .accept_gen(&mut visitor)
      .and_then(|res| visitor.visit_convex(res))
  }
}

#[cfg(test)]
mod tests {
  use crate::{
    common::SpaceTimeRefPos as RefPos,
    space::{
      common::{Frame, FrameRefposFlavor},
      geometry::*,
    },
  };
  use nom::error::VerboseError;

  #[test]
  fn test_circle() {
    let circle = Geometry {
      pre: FillFrameRefposFlavor {
        fillfactor: None,
        frame_refpos_flavor: FrameRefposFlavor {
          frame: Frame::ICRS,
          refpos: Some(RefPos::Geocenter),
          flavor: None,
        },
      },
      params: CircleParams {
        pos: vec![179.0, -11.5],
        radius: 0.5,
      },
      post: FromPosToVelocity {
        position: Some(vec![179.0, -11.5]),
        unit: None,
        error: Some(vec![0.000889]),
        resolution: Some(vec![0.001778]),
        size: Some(vec![0.000333, 0.000278]),
        pixsize: Some(vec![0.000083, 0.000083]),
        velocity: None,
      },
    };
    let s = "Circle ICRS GEOCENTER 179 -11.5 0.5 Position 179 -11.5 Error 0.000889 Resolution 0.001778 Size 0.000333 0.000278 PixSize 0.000083 0.000083";
    assert_eq!(circle.to_string(), s.to_string());

    let (rem, c) = Geometry::<CircleParams>::parse::<VerboseError<&str>>(s).unwrap();
    assert_eq!(circle, c, "{}", rem);

    let json = serde_json::to_string_pretty(&circle).unwrap();
    // println!("{}", json);
    assert_eq!(
      json,
      r#"{
  "frame": "ICRS",
  "refpos": "GEOCENTER",
  "pos": [
    179.0,
    -11.5
  ],
  "radius": 0.5,
  "position": [
    179.0,
    -11.5
  ],
  "error": [
    0.000889
  ],
  "resolution": [
    0.001778
  ],
  "size": [
    0.000333,
    0.000278
  ],
  "pixsize": [
    0.000083,
    0.000083
  ]
}"#
    );
  }
}
