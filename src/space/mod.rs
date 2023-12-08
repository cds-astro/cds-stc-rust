use std::fmt::{self, Display};

use nom::{branch::alt, combinator::map, IResult};

use serde::{Deserialize, Serialize};

use crate::{visitor::SpaceVisitor, NomErr};

pub mod common;
pub mod expression;
pub mod geometry;
pub mod position;
pub mod positioninterval;
use self::{
  common::{
    expression::{DifferenceArgs, IntersectionArgs, NotArgs, UnionArgs},
    region::{AllSkyParams, BoxParams, CircleParams, ConvexParams, EllipseParams, PolygonParams},
  },
  expression::{Expression, ExpressionEnum},
  geometry::{Geometry, GeometryEnum},
  position::Position,
  positioninterval::PositionInterval,
};

/// Space sub-phrase
#[derive(Serialize, Deserialize, Debug, PartialEq)]
#[serde(untagged)]
pub enum Space {
  #[serde(rename = "position_interval")]
  PositionInterval(PositionInterval),
  #[serde(rename = "position")]
  Position(Position),
  #[serde(rename = "geometry")]
  Geometry(GeometryEnum),
  #[serde(rename = "expression")]
  Expression(ExpressionEnum),
}
impl Space {
  pub fn accept<V: SpaceVisitor>(&self, visitor: V) -> Result<V::Value, V::Error> {
    match self {
      Self::Position(e) => visitor.visit_position_simple(e),
      Self::PositionInterval(e) => visitor.visit_position_interval(e),
      Self::Geometry(e) => e.accept(visitor),
      Self::Expression(e) => e.accept(visitor),
    }
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      map(PositionInterval::parse::<E>, Self::PositionInterval),
      map(Position::parse::<E>, Self::Position),
      map(GeometryEnum::parse::<E>, Self::Geometry),
      map(ExpressionEnum::parse::<E>, Self::Expression),
    ))(input)
  }
}
impl Display for Space {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::PositionInterval(e) => Display::fmt(e, f),
      Self::Position(e) => Display::fmt(e, f),
      Self::Geometry(e) => Display::fmt(e, f),
      Self::Expression(e) => Display::fmt(e, f),
    }
  }
}

impl From<PositionInterval> for Space {
  fn from(value: PositionInterval) -> Self {
    Self::PositionInterval(value)
  }
}

impl From<Position> for Space {
  fn from(value: Position) -> Self {
    Self::Position(value)
  }
}

impl From<GeometryEnum> for Space {
  fn from(value: GeometryEnum) -> Self {
    Self::Geometry(value)
  }
}
impl From<Geometry<AllSkyParams>> for Space {
  fn from(value: Geometry<AllSkyParams>) -> Self {
    Self::Geometry(value.into())
  }
}
impl From<Geometry<CircleParams>> for Space {
  fn from(value: Geometry<CircleParams>) -> Self {
    Self::Geometry(value.into())
  }
}
impl From<Geometry<EllipseParams>> for Space {
  fn from(value: Geometry<EllipseParams>) -> Self {
    Self::Geometry(value.into())
  }
}
impl From<Geometry<BoxParams>> for Space {
  fn from(value: Geometry<BoxParams>) -> Self {
    Self::Geometry(value.into())
  }
}
impl From<Geometry<PolygonParams>> for Space {
  fn from(value: Geometry<PolygonParams>) -> Self {
    Self::Geometry(value.into())
  }
}
impl From<Geometry<ConvexParams>> for Space {
  fn from(value: Geometry<ConvexParams>) -> Self {
    Self::Geometry(value.into())
  }
}

impl From<ExpressionEnum> for Space {
  fn from(value: ExpressionEnum) -> Self {
    Self::Expression(value)
  }
}
impl From<Expression<NotArgs>> for Space {
  fn from(value: Expression<NotArgs>) -> Self {
    Self::Expression(value.into())
  }
}
impl From<Expression<UnionArgs>> for Space {
  fn from(value: Expression<UnionArgs>) -> Self {
    Self::Expression(value.into())
  }
}
impl From<Expression<IntersectionArgs>> for Space {
  fn from(value: Expression<IntersectionArgs>) -> Self {
    Self::Expression(value.into())
  }
}
impl From<Expression<DifferenceArgs>> for Space {
  fn from(value: Expression<DifferenceArgs>) -> Self {
    Self::Expression(value.into())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use nom::error::VerboseError;

  #[test]
  fn test_parse_simple() {
    let s = "Circle ICRS TOPOCENTER 147.6 69.9 0.4";
    match Space::parse::<VerboseError<&str>>(s) {
      Ok((rem, space)) => {
        assert_eq!(rem, "", "Remaining: {}", rem);
        // println!("Space: {}", space);
        // println!("Space: {:?}", space);
        assert_eq!(&space.to_string(), s);
        let json = serde_json::to_string_pretty(&space).unwrap();
        // println!("Json:\n{}", json);
        assert_eq!(
          json,
          r#"{
  "Circle": {
    "frame": "ICRS",
    "refpos": "TOPOCENTER",
    "pos": [
      147.6,
      69.9
    ],
    "radius": 0.4
  }
}"#
        );
      }
      Err(err) => {
        println!("Error: {}", err);
        assert!(false)
      }
    }
  }

  #[test]
  fn test_parse_compound_1() {
    let s = r#"Union ICRS TOPOCENTER
(Polygon 147.8 69.2 147.4 69.2 147.3 69.4 147.9 69.4
 Polygon 147.9 69.7 147.6 69.7 147.5 69.9 148.0 69.9)"#;
    match Space::parse::<VerboseError<&str>>(s) {
      Ok((rem, space)) => {
        assert_eq!(rem, "", "Remaining: {}", rem);
        // println!("Space: {}", space);
        // println!("Space: {:?}", space);
        assert_eq!(&space.to_string(), "Union ICRS TOPOCENTER ( Polygon 147.8 69.2 147.4 69.2 147.3 69.4 147.9 69.4 Polygon 147.9 69.7 147.6 69.7 147.5 69.9 148 69.9 )");
        let json = serde_json::to_string_pretty(&space).unwrap();
        // println!("Json:\n{}", json);
        assert_eq!(
          json,
          r#"{
  "Union": {
    "frame": "ICRS",
    "refpos": "TOPOCENTER",
    "elems": [
      {
        "Polygon": {
          "pos": [
            147.8,
            69.2,
            147.4,
            69.2,
            147.3,
            69.4,
            147.9,
            69.4
          ]
        }
      },
      {
        "Polygon": {
          "pos": [
            147.9,
            69.7,
            147.6,
            69.7,
            147.5,
            69.9,
            148.0,
            69.9
          ]
        }
      }
    ]
  }
}"#
        )
      }
      Err(err) => {
        println!("Error: {}", err);
        assert!(false)
      }
    }
  }

  #[test]
  fn test_parse_compound_2() {
    let s = r#"Union ICRS TOPOCENTER
    (Circle 180 10 20
    Circle 190 20 20
    Intersection
     (Circle 120 -10 20
      Difference
       (Circle 130 -10 20
        Circle 125 -10 2
       )
      Not
       (Circle 118 -8 3)
     )
    )"#;
    match Space::parse::<VerboseError<&str>>(s) {
      Ok((rem, space)) => {
        assert_eq!(rem, "", "Remaining: {}", rem);
        // println!("Space: {}", space);
        // println!("Space: {:?}", space);
        assert_eq!(&space.to_string(), "Union ICRS TOPOCENTER ( Circle 180 10 20 Circle 190 20 20 Intersection (  Circle 120 -10 20 Difference ( Circle 130 -10 20 Circle 125 -10 2 ) Not ( Circle 118 -8 3 ) ) )");
        let json = serde_json::to_string_pretty(&space).unwrap();
        // println!("Json:\n{}", json);
        assert_eq!(
          json,
          r#"{
  "Union": {
    "frame": "ICRS",
    "refpos": "TOPOCENTER",
    "elems": [
      {
        "Circle": {
          "pos": [
            180.0,
            10.0
          ],
          "radius": 20.0
        }
      },
      {
        "Circle": {
          "pos": [
            190.0,
            20.0
          ],
          "radius": 20.0
        }
      },
      {
        "Intersection": {
          "elems": [
            {
              "Circle": {
                "pos": [
                  120.0,
                  -10.0
                ],
                "radius": 20.0
              }
            },
            {
              "Difference": {
                "left": {
                  "Circle": {
                    "pos": [
                      130.0,
                      -10.0
                    ],
                    "radius": 20.0
                  }
                },
                "right": {
                  "Circle": {
                    "pos": [
                      125.0,
                      -10.0
                    ],
                    "radius": 2.0
                  }
                }
              }
            },
            {
              "Not": {
                "Circle": {
                  "pos": [
                    118.0,
                    -8.0
                  ],
                  "radius": 3.0
                }
              }
            }
          ]
        }
      }
    ]
  }
}"#
        );
      }
      Err(err) => {
        println!("Error: {}", err);
        assert!(false)
      }
    }
  }
}
