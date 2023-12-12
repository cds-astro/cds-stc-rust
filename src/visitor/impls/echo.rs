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
pub struct EchoVisitor;

impl TimeVisitor for EchoVisitor {
  type Value = ();
  type Error = VoidError;

  fn visit_time_simple(self, time: &TimeSimple) -> Result<Self::Value, Self::Error> {
    println!("visit time simple: {}", time.to_string());
    Ok(())
  }

  fn visit_time_interval(
    self,
    time_interval: &TimeElem<TimeIntervalArgs>,
  ) -> Result<Self::Value, Self::Error> {
    println!("visit time interval: {}", time_interval.to_string());
    Ok(())
  }

  fn visit_time_start(
    self,
    time_start: &TimeElem<TimeStartArgs>,
  ) -> Result<Self::Value, Self::Error> {
    println!("visit time start: {}", time_start.to_string());
    Ok(())
  }

  fn visit_time_stop(self, time_stop: &TimeElem<TimeStopArgs>) -> Result<Self::Value, Self::Error> {
    println!("visit time stop: {}", time_stop.to_string());
    Ok(())
  }
}

impl SpectralVisitor for EchoVisitor {
  type Value = ();
  type Error = VoidError;

  fn visit_spectral_simple(self, value: &SpecValue) -> Result<Self::Value, Self::Error> {
    println!("visit spectral simple: {}", value.to_string());
    Ok(())
  }

  fn visit_spectral_interval(self, interval: &SpecInterval) -> Result<Self::Value, Self::Error> {
    println!("visit spectral interval: {}", interval.to_string());
    Ok(())
  }
}

impl RedshiftVisitor for EchoVisitor {
  type Value = ();
  type Error = VoidError;

  fn visit_redshift_simple(self, redshift: &RedshiftValue) -> Result<Self::Value, Self::Error> {
    println!("visit redshift simple: {}", redshift.to_string());
    Ok(())
  }

  fn visit_redshift_interval(
    self,
    redshift_interval: &RedshiftInterval,
  ) -> Result<Self::Value, Self::Error> {
    println!("visit redshift interval: {}", redshift_interval.to_string());
    Ok(())
  }
}

impl CompoundVisitor for EchoVisitor {
  type Value = ();
  type Error = VoidError;

  fn visit_allsky(&mut self) -> Result<Self::Value, Self::Error> {
    println!(" * cvisit allsky");
    Ok(())
  }

  fn visit_circle(&mut self, circle: &CircleParams) -> Result<Self::Value, Self::Error> {
    println!(" * cvisit circle: {}", circle);
    Ok(())
  }

  fn visit_ellipse(&mut self, ellipse: &EllipseParams) -> Result<Self::Value, Self::Error> {
    println!(" * cvisit ellipse: {}", ellipse);
    Ok(())
  }

  fn visit_box(&mut self, skybox: &BoxParams) -> Result<Self::Value, Self::Error> {
    println!(" * cvisit box: {}", skybox);
    Ok(())
  }

  fn visit_polygon(&mut self, polygon: &PolygonParams) -> Result<Self::Value, Self::Error> {
    println!(" * cvisit polygon: {}", polygon);
    Ok(())
  }

  fn visit_convex(&mut self, convex: &ConvexParams) -> Result<Self::Value, Self::Error> {
    println!(" * cvisit convex: {}", convex);
    Ok(())
  }

  fn visit_not(&mut self, _: Self::Value) -> Result<Self::Value, Self::Error> {
    println!(" * cvisit not");
    Ok(())
  }

  fn visit_union(&mut self, _: Vec<Self::Value>) -> Result<Self::Value, Self::Error> {
    println!(" * cvisit union");
    Ok(())
  }

  fn visit_intersection(&mut self, _: Vec<Self::Value>) -> Result<Self::Value, Self::Error> {
    println!(" * cvisit intersection");
    Ok(())
  }

  fn visit_difference(
    &mut self,
    _: Self::Value,
    _: Self::Value,
  ) -> Result<Self::Value, Self::Error> {
    println!(" * cvisit difference");
    Ok(())
  }
}

impl SpaceVisitor for EchoVisitor {
  type Value = ();
  type Error = VoidError;
  type C = Self;

  fn new_compound_visitor(
    &self,
    fill_fram_refpos_flavor: &FillFrameRefposFlavor,
    from_pos_to_velocity: &FromPosToVelocity,
  ) -> Result<Self, Self::Error> {
    println!(
      "get new compound visitor from params: {}{}",
      fill_fram_refpos_flavor.to_string(),
      from_pos_to_velocity.to_string()
    );
    Ok(Self)
  }

  fn visit_position_simple(self, position: &Position) -> Result<Self::Value, Self::Error> {
    println!("visit position simple: {}", position.to_string());
    Ok(())
  }

  fn visit_position_interval(
    self,
    interval: &PositionInterval,
  ) -> Result<Self::Value, Self::Error> {
    println!("visit position interval: {}", interval.to_string());
    Ok(())
  }

  fn visit_allsky(self, _: Self::Value) -> Result<Self::Value, Self::Error> {
    println!("visit allsky");
    Ok(())
  }

  fn visit_circle(self, _: Self::Value) -> Result<Self::Value, Self::Error> {
    println!("visit circle");
    Ok(())
  }

  fn visit_ellipse(self, _: Self::Value) -> Result<Self::Value, Self::Error> {
    println!("visit ellipse");
    Ok(())
  }

  fn visit_box(self, _: Self::Value) -> Result<Self::Value, Self::Error> {
    println!("visit box");
    Ok(())
  }

  fn visit_polygon(self, _: Self::Value) -> Result<Self::Value, Self::Error> {
    println!("visit polygon");
    Ok(())
  }

  fn visit_convex(self, _: Self::Value) -> Result<Self::Value, Self::Error> {
    println!("visit convex");
    Ok(())
  }

  fn visit_not(self, _: Self::Value) -> Result<Self::Value, Self::Error> {
    println!("visit not");
    Ok(())
  }

  fn visit_union(self, _: Self::Value) -> Result<Self::Value, Self::Error> {
    println!("visit union");
    Ok(())
  }

  fn visit_intersection(self, _: Self::Value) -> Result<Self::Value, Self::Error> {
    println!("visit intersection");
    Ok(())
  }

  fn visit_difference(self, _: Self::Value) -> Result<Self::Value, Self::Error> {
    println!("visit difference");
    Ok(())
  }
}
