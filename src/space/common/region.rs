use std::fmt::{self, Debug, Display};

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::multispace1,
  combinator::{cut, map},
  multi::many_m_n,
  number::complete::double,
  sequence::preceded,
  IResult,
};
use serde::{Deserialize, Serialize};

use crate::NomErr;

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

/// A generic region, defined from its parameters type.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
#[serde(transparent)]
pub struct Region<T: RegionParams>(T);
impl<T: RegionParams> Region<T> {
  pub fn new(params: T) -> Self {
    Self(params)
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(preceded(tag(T::REGION_NAME), cut(T::parse)), Self::new)(input)
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
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E>;
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct AllSkyParams;
impl RegionParams for AllSkyParams {
  const REGION_NAME: &'static str = "AllSky";
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
impl RegionParams for CircleParams {
  const REGION_NAME: &'static str = "Circle";
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
impl RegionParams for EllipseParams {
  const REGION_NAME: &'static str = "Ellipse";
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
impl RegionParams for BoxParams {
  const REGION_NAME: &'static str = "Box";
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
impl RegionParams for PolygonParams {
  const REGION_NAME: &'static str = "Polygon";
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
impl RegionParams for ConvexParams {
  const REGION_NAME: &'static str = "Convex";
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
