# Rapportage Splitsing Overzicht

Deze R Shiny-app laat een gebruiker een of meerdere CSV-bestanden uploaden en maakt daaruit een gecombineerd overzicht met frequentietabellen. Als er meerdere CSV-bestanden worden geselecteerd, worden overeenkomende frequenties bij elkaar opgeteld zodat er een overzicht ontstaat.

De app leest CSV-bestanden altijd in met puntkomma als scheidingsteken.

## Werking

Voor ieder geupload CSV-bestand voert de app de volgende stappen uit:

1. De celwaarden worden opgeschoond door aanhalingstekens en `=`-tekens te verwijderen.
2. De app zoekt naar een cel met `Date of Birth`, ongeacht hoofdletters of kleine letters.
3. De cel direct rechts van `Date of Birth` wordt gebruikt als linkerbovenhoek van het gebied dat moet worden uitgelezen.
4. De app zoekt naar een cel met `Tags`, ongeacht hoofdletters of kleine letters.
5. Vanaf `Tags` wordt naar rechts gezocht totdat een niet-lege cel wordt gevonden.
6. De cel een rij lager en een kolom links van die niet-lege cel wordt gebruikt als rechterbovenhoek van het uit te lezen gebied.
7. De app leest vervolgens rijen naar beneden uit totdat in de geselecteerde kolommen een volledig lege rij wordt gevonden.
8. De eerste uitgelezen rij wordt gebruikt als kolomnamen.
9. Voor iedere uitgelezen kolom wordt een frequentietabel gemaakt.
10. Als meerdere CSV-bestanden zijn geupload, worden frequentietabellen met dezelfde titel gecombineerd.
11. Binnen gecombineerde tabellen worden waarden met dezelfde `Splitsing naam` bij elkaar opgeteld.
12. De kolom `Potentiële deelname` toont de opgetelde oorspronkelijke frequentie.
13. De kolom `Verwachte deelname` wordt berekend met `ceiling(Potentiële deelname * percentage / 100)`.
14. Het percentage is een numeriek invoerveld met een minimum van `0`, maximum van `100` en standaardwaarde `50`.
15. De kolom `Potentiële deelname` krijgt kleurcodering:
    - `0-14`: rood
    - `15-35`: geel
    - `>35`: groen
16. De frequentietabellen worden weergegeven in een responsieve layout met drie tabellen per rij.

## Uitgevoerde wijzigingen

- Een R Shiny-app aangemaakt in `app.R`.
- Uploadfunctionaliteit toegevoegd voor meerdere CSV-bestanden.
- De app ingesteld om CSV-bestanden standaard en uitsluitend met puntkomma te lezen.
- Een Nederlandse uitlegtekst toegevoegd onder het uploadveld.
- De titel van de app gewijzigd naar `Rapportage splitsing overzicht`.
- Opschoning toegevoegd voor quotes en `=`-tekens in geuploade CSV-data.
- Extractielogica toegevoegd op basis van de cellen `Date of Birth` en `Tags`.
- Weergave van ruwe CSV-tabellen vervangen door frequentietabellen.
- Frequentietabellen gecombineerd wanneer meerdere CSV-bestanden worden geupload.
- De oude `Show rows`-toggle verwijderd.
- De kolommen hernoemd naar `Splitsing naam`, `Potentiële deelname` en `Verwachte deelname`.
- Een percentage-invoer toegevoegd waarmee de verwachte deelname wordt berekend.
- De berekening van `Verwachte deelname` aangepast zodat waarden naar boven worden afgerond.
- Kleurcodering toegevoegd voor `Potentiële deelname`.
- De frequentietabellen in een layout van drie per rij geplaatst.

## Starten

Voer vanuit deze map uit:

```r
shiny::runApp("app.R")
```

Als `shiny` nog niet is geinstalleerd:

```r
install.packages("shiny")
```
