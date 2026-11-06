{-# LANGUAGE OverloadedStrings #-}

-- | Waar de 303 van een nieuwsbrief-aanmelding op landt (megavid
-- WebshopScanner.OfferteAanvraag.bedankpaginaVan). Niet de gedeelde
-- offerte-verzonden-pagina: "je hoort van ons" klopt niet voor wie
-- alleen de nieuwsbrief wil. Elke landingspagina komt hier uit, dus de
-- knoppen wijzen niet terug naar één platformpagina. noindex, en niet in
-- de sitemap.
module NieuwsbriefAangemeldPagina
  ( nieuwsbriefAangemeldPagina
  ) where

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import PageChrome (PageMeta(..))
import WebwinkelTemplates (webwinkelBaseTemplate)

nieuwsbriefAangemeldPagina :: Html
nieuwsbriefAangemeldPagina = webwinkelBaseTemplate aangemeldMeta $
  H.main $
    H.section ! A.class_ "hero offerte-blok" $ do
      H.h1 "Je bent aangemeld"
      H.p ! A.class_ "subtitle" $ "Je staat op de lijst voor de nieuwsbrief. Zodra de DIY-exporter klaar is, hoor je het als eerste."
      H.div ! A.class_ "hero-knoppen" $ do
        H.a ! A.href "/" ! A.class_ "cta-button-secondary" $ "Naar de homepagina"
        H.a ! A.href "/scan.html" ! A.class_ "cta-button" $ "Beoordeel alvast je webshop"

aangemeldMeta :: PageMeta
aangemeldMeta = PageMeta
  { pageMetaTitle       = "Aangemeld voor de nieuwsbrief \8212 Webwinkelverhuis"
  , pageMetaDescription = "Je bent aangemeld voor de nieuwsbrief van Webwinkelverhuis."
  , pageMetaLang        = "nl"
  , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/nieuwsbrief-aangemeld.html"
  , pageMetaOgImage     = Nothing
  , pageMetaSwitchUrl   = Nothing
  , pageMetaExtraHead   = H.meta ! A.name "robots" ! A.content "noindex, follow"
  }
