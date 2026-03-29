module Yoga.OpenTelemetry.OpenTelemetryOm
  ( startSpan
  , startSpanWithAttributes
  , endSpan
  , setAttribute
  , addEvent
  , setStatus
  , recordException
  , withSpan
  , withSpanAttrs
  , SpanCtx
  ) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Reader (local)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Yoga.OpenTelemetry.OpenTelemetry as OTel
import Type.Row.Homogeneous (class Homogeneous)
import Yoga.Om as Om

type SpanCtx r = (tracer :: OTel.Tracer, currentSpan :: Maybe OTel.Span | r)

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

withSpan :: forall r err a. String -> Om.Om { | SpanCtx r } err a -> Om.Om { | SpanCtx r } err a
withSpan name action = do
  { tracer, currentSpan } <- Om.ask
  span <- createSpan tracer currentSpan
  catchError (activate span action <* ok span) (fail span)
  where
  createSpan tracer = case _ of
    Nothing -> OTel.startSpan (OTel.SpanName name) tracer # liftEffect
    Just parent -> OTel.startChildSpan (OTel.SpanName name) parent tracer # liftEffect
  activate span = local (_ { currentSpan = Just span })
  ok span = liftEffect do
    OTel.setStatus OTel.StatusOk span
    OTel.endSpan span
  fail span err = do
    liftEffect do
      OTel.recordException name span
      OTel.setStatus OTel.StatusError span
      OTel.endSpan span
    throwError err

withSpanAttrs :: forall r attrs err a. Homogeneous attrs OTel.AttributeValue => String -> { | attrs } -> Om.Om { | SpanCtx r } err a -> Om.Om { | SpanCtx r } err a
withSpanAttrs name attrs action = do
  { tracer, currentSpan } <- Om.ask
  span <- createSpan tracer currentSpan
  catchError (activate span action <* ok span) (fail span)
  where
  createSpan tracer parent = liftEffect do
    span <- case parent of
      Nothing -> OTel.startSpan (OTel.SpanName name) tracer
      Just p -> OTel.startChildSpan (OTel.SpanName name) p tracer
    OTel.setAttributes attrs span
    pure span
  activate span = local (_ { currentSpan = Just span })
  ok span = liftEffect do
    OTel.setStatus OTel.StatusOk span
    OTel.endSpan span
  fail span err = do
    liftEffect do
      OTel.recordException name span
      OTel.setStatus OTel.StatusError span
      OTel.endSpan span
    throwError err
