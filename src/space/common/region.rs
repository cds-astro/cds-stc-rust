use std::fmt::{self, Debug, Display};

use nom::{
  branch::alt,
  bytes::complete::tag_no_case,
  character::complete::multispace1,
  combinator::{cut, map},
  multi::many_m_n,
  number::complete::double,
  sequence::preceded,
  IResult,
};
use serde::{Deserialize, Serialize};

use crate::{visitor::CompoundVisitor, NomErr};

/// Enumeration for all possible `Region`.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum RegionEnum {
  AllSky(Region<AllSkyParams>),
  Circle(Region<CircleParams>),
  Ellipse(Region<EllipseParams>),
  Box(Region<BoxParams>),
  Polygon(Region<PolygonParams>),
  Convex(Region<ConvexParams>),
}
impl RegionEnum {
  pub fn accept<V: CompoundVisitor>(&self, visitor: &mut V) -> Result<V::Value, V::Error> {
    match self {
      Self::AllSky(region) => region.accept(visitor),
      Self::Circle(region) => region.accept(visitor),
      Self::Ellipse(region) => region.accept(visitor),
      Self::Box(region) => region.accept(visitor),
      Self::Polygon(region) => region.accept(visitor),
      Self::Convex(region) => region.accept(visitor),
    }
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      map(Region::<AllSkyParams>::parse, Self::AllSky),
      map(Region::<CircleParams>::parse, Self::Circle),
      map(Region::<EllipseParams>::parse, Self::Ellipse),
      map(Region::<BoxParams>::parse, Self::Box),
      map(Region::<PolygonParams>::parse, Self::Polygon),
      map(Region::<ConvexParams>::parse, Self::Convex),
    ))(input)
  }
}
impl Display for RegionEnum {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::AllSky(region) => Display::fmt(region, f),
      Self::Circle(region) => Display::fmt(region, f),
      Self::Ellipse(region) => Display::fmt(region, f),
      Self::Box(region) => Display::fmt(region, f),
      Self::Polygon(region) => Display::fmt(region, f),
      Self::Convex(region) => Display::fmt(region, f),
    }
  }
}
impl From<Region<AllSkyParams>> for RegionEnum {
  fn from(value: Region<AllSkyParams>) -> Self {
    Self::AllSky(value)
  }
}
impl From<Region<CircleParams>> for RegionEnum {
  fn from(value: Region<CircleParams>) -> Self {
    Self::Circle(value)
  }
}
impl From<Region<EllipseParams>> for RegionEnum {
  fn from(value: Region<EllipseParams>) -> Self {
    Self::Ellipse(value)
  }
}
impl From<Region<BoxParams>> for RegionEnum {
  fn from(value: Region<BoxParams>) -> Self {
    Self::Box(value)
  }
}
impl From<Region<PolygonParams>> for RegionEnum {
  fn from(value: Region<PolygonParams>) -> Self {
    Self::Polygon(value)
  }
}
impl From<Region<ConvexParams>> for RegionEnum {
  fn from(value: Region<ConvexParams>) -> Self {
    Self::Convex(value)
  }
}

/// A generic region, defined from its parameters type.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
#[serde(transparent)]
pub struct Region<T: RegionParams>(T);
impl<T: RegionParams> Region<T> {
  pub fn new(params: T) -> Self {
    Self(params)
  }
  pub fn elem(&self) -> &T {
    &self.0
  }
  pub fn accept<V: CompoundVisitor>(&self, visitor: &mut V) -> Result<V::Value, V::Error> {
    self.elem().accept(visitor)
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(tag_no_case(T::REGION_NAME), cut(T::parse)),
      Self::new,
    )(input)
  }
}
impl<T: RegionParams> Display for Region<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(T::REGION_NAME)?;
    Display::fmt(&self.0, f)
  }
}

/// Trait defining a Region from its name and its parameters parsing and display methods.
pub trait RegionParams: Sized + Display {
  const REGION_NAME: &'static str;
  fn accept<V: CompoundVisitor>(&self, visitor: &mut V) -> Result<V::Value, V::Error>;
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E>;
}

#[derive(Serialize, Deserialize, Default, Debug, PartialEq)]
pub struct AllSkyParams;
/*impl AllSkyParams {
  pub fn new() -> Self {
    Self
  }
}*/
impl RegionParams for AllSkyParams {
  const REGION_NAME: &'static str = "AllSky";
  fn accept<V: CompoundVisitor>(&self, visitor: &mut V) -> Result<V::Value, V::Error> {
    visitor.visit_allsky(self)
  }
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    Ok((input, Self))
  }
}
impl Display for AllSkyParams {
  fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Ok(())
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct CircleParams {
  pub pos: Vec<f64>,
  pub radius: f64,
}
impl CircleParams {
  pub fn new(center: Vec<f64>, radius: f64) -> Self {
    Self {
      pos: center,
      radius,
    }
  }
  pub fn center(&self) -> &[f64] {
    self.pos.as_slice()
  }
  pub fn radius(&self) -> f64 {
    self.radius
  }
}
impl RegionParams for CircleParams {
  const REGION_NAME: &'static str = "Circle";
  fn accept<V: CompoundVisitor>(&self, visitor: &mut V) -> Result<V::Value, V::Error> {
    visitor.visit_circle(self)
  }
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      many_m_n(2, usize::MAX, preceded(multispace1, double)),
      |pos_and_radius| {
        let (radius, pos) = pos_and_radius.split_last().unwrap();
        let radius = *radius;
        let pos = pos.into();
        Self { pos, radius }
      },
    )(input)
  }
}
impl Display for CircleParams {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for pos in self.pos.iter() {
      f.write_fmt(format_args!(" {}", pos))?;
    }
    f.write_fmt(format_args!(" {}", self.radius))
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct EllipseParams {
  pub pos: Vec<f64>,
  pub radius_a: f64,
  pub radius_b: f64,
  pub pos_angle: f64,
}
impl EllipseParams {
  pub fn new(center: Vec<f64>, radius_a: f64, radius_b: f64, pos_angle: f64) -> Self {
    Self {
      pos: center,
      radius_a,
      radius_b,
      pos_angle,
    }
  }
  pub fn center(&self) -> &[f64] {
    self.pos.as_slice()
  }
  pub fn radius_a(&self) -> f64 {
    self.radius_a
  }
  pub fn radius_b(&self) -> f64 {
    self.radius_b
  }
  pub fn pos_angle(&self) -> f64 {
    self.pos_angle
  }
}
impl RegionParams for EllipseParams {
  const REGION_NAME: &'static str = "Ellipse";
  fn accept<V: CompoundVisitor>(&self, visitor: &mut V) -> Result<V::Value, V::Error> {
    visitor.visit_ellipse(self)
  }
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      many_m_n(4, usize::MAX, preceded(multispace1, double)),
      |pos_and_radii| {
        let (pos_angle, pos) = pos_and_radii.split_last().unwrap();
        let (radius_b, pos) = pos.split_last().unwrap();
        let (radius_a, pos) = pos.split_last().unwrap();
        Self {
          pos: pos.into(),
          radius_a: *radius_a,
          radius_b: *radius_b,
          pos_angle: *pos_angle,
        }
      },
    )(input)
  }
}
impl Display for EllipseParams {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for pos in self.pos.iter() {
      f.write_fmt(format_args!(" {}", pos))?;
    }
    f.write_fmt(format_args!(" {}", self.radius_a))?;
    f.write_fmt(format_args!(" {}", self.radius_b))?;
    f.write_fmt(format_args!(" {}", self.pos_angle))
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct BoxParams {
  pub pos: Vec<f64>,
  pub bsize: Vec<f64>,
}
impl BoxParams {
  pub fn new(center: Vec<f64>, bsize: Vec<f64>) -> Self {
    Self { pos: center, bsize }
  }
  pub fn center(&self) -> &[f64] {
    self.pos.as_slice()
  }
  pub fn bsize(&self) -> &[f64] {
    self.bsize.as_slice()
  }
}
impl RegionParams for BoxParams {
  const REGION_NAME: &'static str = "Box";
  fn accept<V: CompoundVisitor>(&self, visitor: &mut V) -> Result<V::Value, V::Error> {
    visitor.visit_box(self)
  }
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      many_m_n(2, usize::MAX, preceded(multispace1, double)),
      |pos_and_bsize| {
        let (pos, bsize) = pos_and_bsize.split_at(pos_and_bsize.len() >> 1);
        Self {
          pos: pos.into(),
          bsize: bsize.into(),
        }
      },
    )(input)
  }
}
impl Display for BoxParams {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for pos in self.pos.iter() {
      f.write_fmt(format_args!(" {}", pos))?;
    }
    for bsize in self.bsize.iter() {
      f.write_fmt(format_args!(" {}", bsize))?;
    }
    Ok(())
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct PolygonParams {
  pub pos: Vec<f64>,
}
impl PolygonParams {
  pub fn new(vertices: Vec<f64>) -> Self {
    Self { pos: vertices }
  }
  pub fn vertices(&self) -> &[f64] {
    self.pos.as_slice()
  }
}
impl RegionParams for PolygonParams {
  const REGION_NAME: &'static str = "Polygon";
  fn accept<V: CompoundVisitor>(&self, visitor: &mut V) -> Result<V::Value, V::Error> {
    visitor.visit_polygon(self)
  }
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      many_m_n(3, usize::MAX, preceded(multispace1, double)),
      |pos| Self { pos },
    )(input)
  }
}
impl Display for PolygonParams {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for pos in self.pos.iter() {
      f.write_fmt(format_args!(" {}", pos))?;
    }
    Ok(())
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct ConvexParams {
  pub hspace: Vec<f64>,
}
impl ConvexParams {
  pub fn new(hspace: Vec<f64>) -> Self {
    Self { hspace }
  }
  pub fn hspace(&self) -> &[f64] {
    self.hspace.as_slice()
  }
}
impl RegionParams for ConvexParams {
  const REGION_NAME: &'static str = "Convex";
  fn accept<V: CompoundVisitor>(&self, visitor: &mut V) -> Result<V::Value, V::Error> {
    visitor.visit_convex(self)
  }
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      many_m_n(3, usize::MAX, preceded(multispace1, double)),
      |hspace| Self { hspace },
    )(input)
  }
}
impl Display for ConvexParams {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for pos in self.hspace.iter() {
      f.write_fmt(format_args!(" {}", pos))?;
    }
    Ok(())
  }
}
