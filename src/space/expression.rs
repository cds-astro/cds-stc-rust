use std::fmt::{self, Display};

use nom::{
  branch::alt,
  bytes::complete::tag_no_case,
  character::complete::{char, multispace0},
  combinator::{cut, map},
  sequence::{delimited, preceded, tuple},
  IResult,
};
use serde::{Deserialize, Serialize};

use super::common::{
  expression::{DifferenceArgs, ExprArgs, IntersectionArgs, NotArgs, UnionArgs},
  velocity::Velocity,
  FillFrameRefposFlavor, Flavor, Frame, FromPosToVelocity, SpaceUnit,
};
use crate::{
  common::SpaceTimeRefPos,
  space::common::compound::RegionOrExpr,
  visitor::{CompoundVisitor, SpaceVisitor},
  NomErr,
};

/// Enumeration for all possible `Expressions`.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ExpressionEnum {
  Not(Expression<NotArgs>),
  Union(Expression<UnionArgs>),
  Intersection(Expression<IntersectionArgs>),
  Difference(Expression<DifferenceArgs>),
}
impl ExpressionEnum {
  pub fn accept<V: SpaceVisitor>(&self, visitor: V) -> Result<V::Value, V::Error> {
    match self {
      Self::Not(expr) => expr.accept(visitor),
      Self::Union(expr) => expr.accept(visitor),
      Self::Intersection(expr) => expr.accept(visitor),
      Self::Difference(expr) => expr.accept(visitor),
    }
  }

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
impl From<Expression<NotArgs>> for ExpressionEnum {
  fn from(value: Expression<NotArgs>) -> Self {
    Self::Not(value)
  }
}
impl From<Expression<UnionArgs>> for ExpressionEnum {
  fn from(value: Expression<UnionArgs>) -> Self {
    Self::Union(value)
  }
}
impl From<Expression<IntersectionArgs>> for ExpressionEnum {
  fn from(value: Expression<IntersectionArgs>) -> Self {
    Self::Intersection(value)
  }
}
impl From<Expression<DifferenceArgs>> for ExpressionEnum {
  fn from(value: Expression<DifferenceArgs>) -> Self {
    Self::Difference(value)
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

  pub fn from_frame_and_args(frame: Frame, args: T) -> Self {
    Self::new(
      FillFrameRefposFlavor::from_frame(frame),
      args,
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
  pub fn args(&self) -> &T {
    &self.args
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
      .and_then(|mut visitor| self.args.accept(&mut visitor))
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag_no_case(T::EXPR_NAME),
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
impl Expression<NotArgs> {
  pub fn new_not<T: Into<RegionOrExpr>>(frame: Frame, arg: T) -> Self {
    Self::from_frame_and_args(frame, NotArgs::new(arg))
  }
  pub fn elem(&self) -> &RegionOrExpr {
    self.args.elem()
  }
  pub fn accept<V: SpaceVisitor>(&self, mut visitor: V) -> Result<V::Value, V::Error> {
    self
      .accept_gen(&mut visitor)
      .and_then(|res| visitor.visit_not(res))
  }
}
impl Expression<DifferenceArgs> {
  pub fn new_difference<L, R>(frame: Frame, left: L, right: R) -> Self
  where
    L: Into<RegionOrExpr>,
    R: Into<RegionOrExpr>,
  {
    Self::from_frame_and_args(frame, DifferenceArgs::new(left, right))
  }
  pub fn left_elem(&self) -> &RegionOrExpr {
    self.args.left_elem()
  }
  pub fn right_elem(&self) -> &RegionOrExpr {
    self.args.right_elem()
  }
  pub fn accept<V: SpaceVisitor>(&self, mut visitor: V) -> Result<V::Value, V::Error> {
    self
      .accept_gen(&mut visitor)
      .and_then(|res| visitor.visit_difference(res))
  }
}
impl Expression<UnionArgs> {
  pub fn new_union(frame: Frame, arg: Vec<RegionOrExpr>) -> Self {
    Self::from_frame_and_args(frame, UnionArgs::new(arg))
  }
  pub fn elems(&self) -> &[RegionOrExpr] {
    self.args.elems()
  }
  pub fn accept<V: SpaceVisitor>(&self, mut visitor: V) -> Result<V::Value, V::Error> {
    self
      .accept_gen(&mut visitor)
      .and_then(|res| visitor.visit_union(res))
  }
}
impl Expression<IntersectionArgs> {
  pub fn new_union(frame: Frame, arg: Vec<RegionOrExpr>) -> Self {
    Self::from_frame_and_args(frame, IntersectionArgs::new(arg))
  }
  pub fn elems(&self) -> &[RegionOrExpr] {
    self.args.elems()
  }
  pub fn accept<V: SpaceVisitor>(&self, mut visitor: V) -> Result<V::Value, V::Error> {
    self
      .accept_gen(&mut visitor)
      .and_then(|res| visitor.visit_intersection(res))
  }
}
