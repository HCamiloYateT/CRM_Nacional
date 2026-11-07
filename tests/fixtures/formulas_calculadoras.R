.inp0 <- function(x) if (is.null(x) || is.na(x)) 0 else x
.esperado_CalculadoraCoproductos <- function(input) {
data.frame(val_9d3 = input$COSTO_9D3, val_9d1 = input$COSTO_9D1, val_5d50 = input$COSTO_5D50, val_5d5 = input$COSTO_5D5, 
    val_1d7 = input$COSTO_1D7, Trilladora = input$CostosTrilladora, GastosFijos = input$GastosFijos, Fletes = input$Fletes, 
    MargenEsperado = input$MargenEsperado, Pre9D3 = .inp0(input$Pre9D3), Pre9D1 = .inp0(input$Pre9D1), Pre5D50 = .inp0(input$Pre5D50), 
    Pre5D5 = .inp0(input$Pre5D5), Pre1D7 = .inp0(input$Pre1D7), Kls9D3 = .inp0(input$Kls9D3), Kls9D1 = .inp0(input$Kls9D1), 
    Kls5D50 = .inp0(input$Kls5D50), Kls5D5 = .inp0(input$Kls5D5), Kls1D7 = .inp0(input$Kls1D7)) %>% mutate(Precio_9d3 = val_9d3 + 
    Trilladora + GastosFijos + Fletes + MargenEsperado, Precio_9d1 = val_9d1 + Trilladora + GastosFijos + Fletes + 
    MargenEsperado, Precio_5d50 = val_5d50 + Trilladora + GastosFijos + Fletes + MargenEsperado, Precio_5d5 = val_5d5 + 
    Trilladora + GastosFijos + Fletes + MargenEsperado, Precio_1d7 = val_1d7 + Trilladora + GastosFijos + Fletes + 
    MargenEsperado, Mar9D3 = Pre9D3 - Precio_9d3, Mar9D1 = Pre9D1 - Precio_9d1, Mar5D50 = Pre5D50 - Precio_5d50, 
    Mar5D5 = Pre5D5 - Precio_5d5, Mar1D7 = Pre1D7 - Precio_1d7, Utl9D3 = Mar9D3 * Kls9D3, Utl9D1 = Mar9D1 * Kls9D1, 
    Utl5D50 = Mar5D50 * Kls5D50, Utl5D5 = Mar5D5 * Kls5D5, Utl1D7 = Mar1D7 * Kls1D7)
}
.esperado_CalculadoraDolaresExcelso <- function(input) {
data.frame(TRM = input$TRM, NYC = input$NYC, PrimaUGQ = input$PrimaUGQ, PrimaMallas = input$PrimaMallas, PrimaEspeciales = input$PrimaEspeciales, 
    Utilidad = input$Utilidad, Empaque = input$Empaque, Grainpro = input$Grainpro, Flete = input$Flete) %>% mutate(Total = NYC + 
    PrimaUGQ + PrimaMallas + PrimaEspeciales + Utilidad + Empaque + Grainpro, PrecioUSD = Total * 2.20462/100, 
    PrecioCOP = PrecioUSD * TRM, PrecioFlete = PrecioCOP + Flete)
}
.esperado_CalculadoraPesosExcelso <- function(input) {
data.frame(TRM = input$TRM, CostoConsumo = input$CostoConsumo, PrecioCarga = input$PrecioCarga, PrimaEspecial = input$PrimaEspecial, 
    PrimaSupremo = .inp0(input$PrimaSupremo), MarBlend = .inp0(input$MarBlend), MarUGQ = .inp0(input$MarUGQ), MarSup = .inp0(input$MarSup), 
    MarEsp = .inp0(input$MarEsp), Prima = input$Prima, FleteInt = input$FleteInt, FleteExt = input$FleteExt, CostosFijos = input$CostosFijos, 
    Financieros = input$Financieros, Precio = input$Precio) %>% mutate(PrecioKilo = PrecioCarga/125, Rendimiento = 70/96.89, 
    CostoKilo = PrecioKilo/Rendimiento, Costos = CostoKilo + PrimaEspecial + FleteInt + Financieros, CosUGQ = CostoKilo, 
    PrecioUGQ = CosUGQ + MarUGQ, UtilidadKilo = PrecioUGQ - Costos, UtilidadCtvs = ((UtilidadKilo * 70)/1.54322)/TRM, 
    CosBlend = (CostoConsumo * 0.4) + (CosUGQ * 0.6), PreBlend = CosBlend + MarBlend, UtBlend = (((PreBlend - CosBlend) * 
        70)/1.54322)/TRM, PreUGQ = CosUGQ + MarUGQ, UtUGQ = (((PreUGQ - CosUGQ) * 70)/1.54322)/TRM, CosSup = CosUGQ + 
        PrimaSupremo, PreSup = CosSup + MarSup, UtSup = (((PreSup - CosSup) * 70)/1.54322)/TRM, CosEsp = CosUGQ + 
        PrimaEspecial, PreEsp = CosEsp + MarEsp, UtEsp = (((PreEsp - CosEsp) * 70)/1.54322)/TRM, Costos2 = CostoKilo + 
        Prima + FleteInt + FleteExt + CostosFijos + Financieros, Utilidad2 = Precio - Costos2, UtilidadCtvs2 = ((Utilidad2 * 
        70)/1.54322)/TRM)
}
