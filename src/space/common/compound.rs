use std::fmt::{self, Debug, Display};

use nom::{branch::alt, combinator::map, IResult};
use serde::{Deserialize, Serialize};

use crate::NomErr;
use super::expression::ExprEnum;
use super::region::RegionEnum;

/// Enumeration for all either a `Region` or `Expression`.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
#[serde(untagged)]
pub enum RegionOrExpr {
  Region(RegionEnum),
  Expression(ExprEnum),
}
impl RegionOrExpr {
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
