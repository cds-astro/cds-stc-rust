use std::fmt::{self, Debug, Display};

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{char, multispace0, multispace1},
  combinator::{cut, map},
  multi::separated_list1,
  sequence::{delimited, preceded, separated_pair},
  IResult,
};
use serde::{Deserialize, Serialize};

use super::compound::RegionOrExpr;
use crate::NomErr;

/// Enumeration for all possible `Expressions`:
/// * `Not`
/// * `Union`
/// * `Intersection`
/// * `Difference`
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ExprEnum {
  Not(Expr<NotArgs>),
  Union(Expr<UnionArgs>),
  Intersection(Expr<IntersectionArgs>),
  Difference(Expr<DifferenceArgs>),
}
impl ExprEnum {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      map(Expr::<NotArgs>::parse::<'a, E>, Self::Not),
      map(Expr::<UnionArgs>::parse::<'a, E>, Self::Union),
      map(Expr::<IntersectionArgs>::parse::<'a, E>, Self::Intersection),
      map(Expr::<DifferenceArgs>::parse::<'a, E>, Self::Difference),
    ))(input)
  }
}
impl Display for ExprEnum {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Not(expr) => Display::fmt(expr, f),
      Self::Union(expr) => Display::fmt(expr, f),
      Self::Intersection(expr) => Display::fmt(expr, f),
      Self::Difference(expr) => Display::fmt(expr, f),
    }
  }
}

/// A generic expression, defined from its arguments type.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
#[serde(transparent)]
pub struct Expr<T: ExprArgs>(T);
impl<T: ExprArgs> Expr<T> {
  pub fn new(params: T) -> Self {
    Self(params)
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag(T::EXPR_NAME),
        delimited(
          delimited(multispace0, char('('), multispace0),
          cut(T::parse::<'a, E>),
          preceded(multispace0, char(')')),
        ),
      ),
      Self::new,
    )(input)
  }
}
impl<T: ExprArgs> Display for Expr<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_fmt(format_args!("{} ( {} )", T::EXPR_NAME, &self.0))
  }
}

/// Trait defining a Region from its name and its parameters parsing and display methods.
pub trait ExprArgs: Sized + Display {
  const EXPR_NAME: &'static str;
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E>;
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct NotArgs(Box<RegionOrExpr>);
impl ExprArgs for NotArgs {
  const EXPR_NAME: &'static str = "Not";
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(RegionOrExpr::parse::<E>, |e| Self(Box::new(e)))(input)
  }
}
impl Display for NotArgs {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Display::fmt(&self.0, f)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct DifferenceArgs {
  left: Box<RegionOrExpr>,
  right: Box<RegionOrExpr>,
}
impl ExprArgs for DifferenceArgs {
  const EXPR_NAME: &'static str = "Difference";
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      separated_pair(
        RegionOrExpr::parse::<'a, E>,
        multispace1,
        RegionOrExpr::parse::<'a, E>,
      ),
      |(left, right)| Self {
        left: Box::new(left),
        right: Box::new(right),
      },
    )(input)
  }
}
impl Display for DifferenceArgs {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_fmt(format_args!("{} {}", self.left, self.right))
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct UnionArgs {
  elems: Vec<RegionOrExpr>,
}
impl UnionArgs {
  pub fn new(elems: Vec<RegionOrExpr>) -> Self {
    Self { elems }
  }
}
impl ExprArgs for UnionArgs {
  const EXPR_NAME: &'static str = "Union";
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      separated_list1(multispace1, RegionOrExpr::parse::<E>),
      Self::new,
    )(input)
  }
}
impl Display for UnionArgs {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for r in &self.elems {
      f.write_fmt(format_args!(" {}", r))?;
    }
    Ok(())
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct IntersectionArgs {
  elems: Vec<RegionOrExpr>,
}
impl IntersectionArgs {
  pub fn new(elems: Vec<RegionOrExpr>) -> Self {
    Self { elems }
  }
}
impl ExprArgs for IntersectionArgs {
  const EXPR_NAME: &'static str = "Intersection";
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      separated_list1(multispace1, RegionOrExpr::parse::<E>),
      Self::new,
    )(input)
  }
}
impl Display for IntersectionArgs {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for r in &self.elems {
      f.write_fmt(format_args!(" {}", r))?;
    }
    Ok(())
  }
}
