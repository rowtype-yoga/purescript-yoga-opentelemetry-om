module Yoga.OpenTelemetry.OpenTelemetryOm
  ( startSpan
  , startSpanWithAttributes
  , endSpan
  , setAttribute
  , addEvent
  , setStatus
  , recordException
  , withSpan
  ) where

import Prelude

import Effect.Class (liftEffect)
import Yoga.OpenTelemetry.OpenTelemetry as OTel
import Type.Row.Homogeneous (class Homogeneous)
import Yoga.Om as Om

-- All tracing functions automatically get tracer from Om context!

startSpan :: forall r err. String -> Om.Om { tracer :: OTel.Tracer | r } err OTel.Span
startSpan name = do
  { tracer } <- Om.ask
  liftEffect $ OTel.startSpan (OTel.SpanName name) tracer

startSpanWithAttributes :: forall r attrs err. Homogeneous attrs OTel.AttributeValue => String -> { | attrs } -> Om.Om { tracer :: OTel.Tracer | r } err OTel.Span
startSpanWithAttributes name attrs = do
  { tracer } <- Om.ask
  liftEffect $ OTel.startSpanWithAttributes (OTel.SpanName name) attrs tracer

endSpan :: forall r err. OTel.Span -> Om.Om r err Unit
endSpan span = liftEffect $ OTel.endSpan span

setAttribute :: forall r err. String -> OTel.AttributeValue -> OTel.Span -> Om.Om r err Unit
setAttribute key value span = liftEffect $ OTel.setAttribute key value span

addEvent :: forall r err. String -> OTel.Span -> Om.Om r err Unit
addEvent name span = liftEffect $ OTel.addEvent name span

setStatus :: forall r err. OTel.SpanStatusCode -> OTel.Span -> Om.Om r err Unit
setStatus status span = liftEffect $ OTel.setStatus status span

recordException :: forall r err. String -> OTel.Span -> Om.Om r err Unit
recordException error span = liftEffect $ OTel.recordException error span

-- Helper: Run an action within a span (automatically ends it)
withSpan :: forall r err a. String -> Om.Om { tracer :: OTel.Tracer | r } err a -> Om.Om { tracer :: OTel.Tracer | r } err a
withSpan name action = do
  span <- startSpan name
  result <- action
  endSpan span
  pure result
