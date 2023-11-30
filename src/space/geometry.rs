use std::fmt::{self, Debug, Display};

use nom::{
  branch::alt,
  bytes::complete::tag,
  combinator::{cut, map},
  sequence::{preceded, tuple},
  IResult,
};
use serde::{Deserialize, Serialize};

use super::common::{
  region::{
    AllSkyParams, BoxParams, CircleParams, ConvexParams, EllipseParams, PolygonParams, RegionParams,
  },
  FillFrameRefposFlavor, FromPosToVelocity,
};
use crate::NomErr;

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
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag(T::REGION_NAME),
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
