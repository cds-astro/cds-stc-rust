use std::fmt::{self, Display};

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{char, multispace0},
  combinator::{cut, map},
  sequence::{delimited, preceded, tuple},
  IResult,
};
use serde::{Deserialize, Serialize};

use super::common::{
  expression::{DifferenceArgs, ExprArgs, IntersectionArgs, NotArgs, UnionArgs},
  FillFrameRefposFlavor, FromPosToVelocity,
};
use crate::NomErr;

/// Enumeration for all possible `Expressions`.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ExpressionEnum {
  Not(Expression<NotArgs>),
  Union(Expression<UnionArgs>),
  Intersection(Expression<IntersectionArgs>),
  Difference(Expression<DifferenceArgs>),
}
impl ExpressionEnum {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      map(Expression::<NotArgs>::parse, Self::Not),
      map(Expression::<UnionArgs>::parse, Self::Union),
      map(Expression::<IntersectionArgs>::parse, Self::Intersection),
      map(Expression::<DifferenceArgs>::parse, Self::Difference),
    ))(input)
  }
}
impl Display for ExpressionEnum {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Not(expr) => Display::fmt(expr, f),
      Self::Union(expr) => Display::fmt(expr, f),
      Self::Intersection(expr) => Display::fmt(expr, f),
      Self::Difference(expr) => Display::fmt(expr, f),
    }
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Expression<T: ExprArgs> {
  #[serde(flatten)]
  pub pre: FillFrameRefposFlavor,
  #[serde(flatten)]
  pub args: T,
  #[serde(flatten)]
  pub post: FromPosToVelocity,
}
impl<T: ExprArgs> Expression<T> {
  pub fn new(pre: FillFrameRefposFlavor, args: T, post: FromPosToVelocity) -> Self {
    Self { pre, args, post }
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag(T::EXPR_NAME),
        tuple((
          FillFrameRefposFlavor::parse,
          delimited(
            delimited(multispace0, char('('), multispace0),
            cut(T::parse),
            preceded(multispace0, char(')')),
          ),
          FromPosToVelocity::parse,
        )),
      ),
      |(pre, args, post)| Self { pre, args, post },
    )(input)
  }
}
impl<T: ExprArgs> Display for Expression<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(T::EXPR_NAME)?;
    Display::fmt(&self.pre, f)?;
    f.write_fmt(format_args!(" ({} )", &self.args))?;
    Display::fmt(&self.post, f)
  }
}
