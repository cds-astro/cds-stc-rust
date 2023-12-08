use std::fmt::{self, Debug, Display};

use nom::{branch::alt, combinator::map, IResult};
use serde::{Deserialize, Serialize};

use super::{
  expression::{DifferenceArgs, Expr, ExprEnum, IntersectionArgs, NotArgs, UnionArgs},
  region::{
    AllSkyParams, BoxParams, CircleParams, ConvexParams, EllipseParams, PolygonParams, Region,
    RegionEnum,
  },
};
use crate::{visitor::CompoundVisitor, NomErr};

/// Enumeration for all either a `Region` or `Expression`.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
#[serde(untagged)]
pub enum RegionOrExpr {
  Region(RegionEnum),
  Expression(ExprEnum),
}
impl RegionOrExpr {
  pub fn accept<V: CompoundVisitor>(&self, visitor: &mut V) -> Result<V::Value, V::Error> {
    match self {
      Self::Region(expr) => expr.accept(visitor),
      Self::Expression(expr) => expr.accept(visitor),
    }
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      map(RegionEnum::parse::<E>, RegionOrExpr::Region),
      map(ExprEnum::parse::<E>, RegionOrExpr::Expression),
    ))(input)
  }
}
impl Display for RegionOrExpr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Self::Region(region) => Display::fmt(region, f),
      Self::Expression(expression) => Display::fmt(expression, f),
    }
  }
}
impl From<RegionEnum> for RegionOrExpr {
  fn from(value: RegionEnum) -> Self {
    Self::Region(value)
  }
}
impl From<Region<AllSkyParams>> for RegionOrExpr {
  fn from(value: Region<AllSkyParams>) -> Self {
    Self::Region(value.into())
  }
}
impl From<Region<CircleParams>> for RegionOrExpr {
  fn from(value: Region<CircleParams>) -> Self {
    Self::Region(value.into())
  }
}
impl From<Region<EllipseParams>> for RegionOrExpr {
  fn from(value: Region<EllipseParams>) -> Self {
    Self::Region(value.into())
  }
}
impl From<Region<BoxParams>> for RegionOrExpr {
  fn from(value: Region<BoxParams>) -> Self {
    Self::Region(value.into())
  }
}
impl From<Region<PolygonParams>> for RegionOrExpr {
  fn from(value: Region<PolygonParams>) -> Self {
    Self::Region(value.into())
  }
}
impl From<Region<ConvexParams>> for RegionOrExpr {
  fn from(value: Region<ConvexParams>) -> Self {
    Self::Region(value.into())
  }
}

impl From<ExprEnum> for RegionOrExpr {
  fn from(value: ExprEnum) -> Self {
    Self::Expression(value)
  }
}
impl From<Expr<NotArgs>> for RegionOrExpr {
  fn from(value: Expr<NotArgs>) -> Self {
    Self::Expression(value.into())
  }
}
impl From<Expr<UnionArgs>> for RegionOrExpr {
  fn from(value: Expr<UnionArgs>) -> Self {
    Self::Expression(value.into())
  }
}
impl From<Expr<IntersectionArgs>> for RegionOrExpr {
  fn from(value: Expr<IntersectionArgs>) -> Self {
    Self::Expression(value.into())
  }
}
impl From<Expr<DifferenceArgs>> for RegionOrExpr {
  fn from(value: Expr<DifferenceArgs>) -> Self {
    Self::Expression(value.into())
  }
}
