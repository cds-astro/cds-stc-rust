use std::{
  error::Error,
  fmt::{Display, Formatter},
};

use crate::{
  redshift::{RedshiftInterval, RedshiftValue},
  space::{
    common::{
      region::{BoxParams, CircleParams, ConvexParams, EllipseParams, PolygonParams},
      FillFrameRefposFlavor, FromPosToVelocity,
    },
    position::Position,
    positioninterval::PositionInterval,
  },
  spectral::{SpecInterval, SpecValue},
  time::{TimeElem, TimeIntervalArgs, TimeSimple, TimeStartArgs, TimeStopArgs},
  visitor::{CompoundVisitor, RedshiftVisitor, SpaceVisitor, SpectralVisitor, TimeVisitor},
};

#[derive(Debug)]
pub struct VoidError;

impl Display for VoidError {
  fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
    Ok(())
  }
}

impl Error for VoidError {}

/// Dummy implementation of `TimeVisitor` doing... nothing
/// (to be used when we are not interested in the Time part in a STC-S string).
pub struct VoidVisitor;

impl TimeVisitor for VoidVisitor {
  type Value = ();
  type Error = VoidError;

  fn visit_time_simple(self, _time: &TimeSimple) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_time_interval(
    self,
    _time_interval: &TimeElem<TimeIntervalArgs>,
  ) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_time_start(
    self,
    _time_start: &TimeElem<TimeStartArgs>,
  ) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_time_stop(
    self,
    _time_stop: &TimeElem<TimeStopArgs>,
  ) -> Result<Self::Value, Self::Error> {
    Ok(())
  }
}

impl SpectralVisitor for VoidVisitor {
  type Value = ();
  type Error = VoidError;

  fn visit_spectral_simple(self, _value: &SpecValue) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_spectral_interval(self, _interval: &SpecInterval) -> Result<Self::Value, Self::Error> {
    Ok(())
  }
}

impl RedshiftVisitor for VoidVisitor {
  type Value = ();
  type Error = VoidError;

  fn visit_redshift_simple(self, _redshift: &RedshiftValue) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_redshift_interval(
    self,
    _redshift_interval: &RedshiftInterval,
  ) -> Result<Self::Value, Self::Error> {
    Ok(())
  }
}

impl CompoundVisitor for VoidVisitor {
  type Value = ();
  type Error = VoidError;

  fn visit_allsky(&mut self) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_circle(&mut self, _circle: &CircleParams) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_ellipse(&mut self, _ellipse: &EllipseParams) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_box(&mut self, _skybox: &BoxParams) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_polygon(&mut self, _polygon: &PolygonParams) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_convex(&mut self, _convex: &ConvexParams) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_not(&mut self, _content_visit_result: Self::Value) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_union(
    &mut self,
    _content_visit_result: Vec<Self::Value>,
  ) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_intersection(
    &mut self,
    _content_visit_result: Vec<Self::Value>,
  ) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_difference(
    &mut self,
    _left_visit_result: Self::Value,
    _right_visit_result: Self::Value,
  ) -> Result<Self::Value, Self::Error> {
    Ok(())
  }
}

impl SpaceVisitor for VoidVisitor {
  type Value = ();
  type Error = VoidError;
  type C = Self;

  fn new_compound_visitor(
    &self,
    _fill_fram_refpos_flavor: &FillFrameRefposFlavor,
    _from_pos_to_velocity: &FromPosToVelocity,
  ) -> Result<Self, Self::Error> {
    Ok(Self)
  }

  fn visit_position_simple(self, _position: &Position) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_position_interval(
    self,
    _interval: &PositionInterval,
  ) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_allsky(self, _content_visit_result: Self::Value) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_circle(self, _content_visit_result: Self::Value) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_ellipse(self, _content_visit_result: Self::Value) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_box(self, _content_visit_result: Self::Value) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_polygon(self, _content_visit_result: Self::Value) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_convex(self, _content_visit_result: Self::Value) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_not(self, _content_visit_result: Self::Value) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_union(self, _content_visit_result: Self::Value) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_intersection(
    self,
    _content_visit_result: Self::Value,
  ) -> Result<Self::Value, Self::Error> {
    Ok(())
  }

  fn visit_difference(
    self,
    _content_visit_result: Self::Value,
  ) -> Result<Self::Value, Self::Error> {
    Ok(())
  }
}
