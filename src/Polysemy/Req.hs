{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      :  Polysemy.Req
-- Copyright   :  (C) 2021 Morrow
-- License     :  BSD3-3-Clause
-- Maintainer  :  Morrow <themorrowm@gmail.com>
-- "Network.HTTP.Req" adapted for use with polysemy.
module Polysemy.Req
  ( -- * Effect
    Req (..),

    -- * Actions
    req,

    -- * Interpretations
    interpretReq,
    interpretReqWith,

    -- * Re-exports
    module Network.HTTP.Req,
  )
where

import Data.Proxy
import Network.HTTP.Req hiding (MonadHttp, Req, req, req', reqBr, reqCb, runReq)
import qualified Network.HTTP.Req as R
import Polysemy

-- | An effect for making http 'Network.HTTP.Req.req'uests.
data Req m response where
  Req ::
    ( HttpMethod method,
      HttpBody body,
      HttpResponse response,
      HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
    ) =>
    -- | HTTP method
    method ->
    -- | 'Url'—location of resource
    Url scheme ->
    -- | Body of the request
    body ->
    -- | A hint how to interpret response
    Proxy response ->
    -- | Collection of optional parameters
    Option scheme ->
    -- | Response
    Req m response

-- | See 'Network.HTTP.Req.req'.
makeSem ''Req

-- | Run a 'Req' effect with the 'Network.HTTP.Req.defaultHttpConfig'.
interpretReq :: Member (Embed IO) r => InterpreterFor Req r
interpretReq = interpretReqWith defaultHttpConfig

-- | Run a 'Req' effect with a custom 'Network.HTTP.Req.HttpConfig'.
interpretReqWith :: Member (Embed IO) r => HttpConfig -> InterpreterFor Req r
interpretReqWith cfg = interpret $ \case
  Req m u b p o -> embed @IO $ R.runReq cfg $ R.req @R.Req m u b p o
