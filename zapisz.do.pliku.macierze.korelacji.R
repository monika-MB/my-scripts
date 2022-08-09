zapisz.do.pliku.macierze.korelacji<<-function(macierz.rpn,macierz.p,macierz.r,sheet,nazwa.cech.w.wierszach="Cechy 1",nazwa.cech.w.kolumnach="Cechy 2",wb=NULL,alfa.do.pliku=0.05){

  library("grDevices")
  paleta=rev(palette(heat.colors(9)))

  cs1 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border() + Alignment(wrapText = TRUE)# header
  cs2 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border(position = "RIGHT") +Alignment(horizontal="ALIGN_RIGHT",wrapText = TRUE) # header
  cs3 <- CellStyle(wb) + Fill(foregroundColor = "yellow") +Alignment(wrapText = TRUE)+Font(wb, isBold=TRUE)
  cs4 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")+Border(position = c("TOP","BOTTOM","LEFT","RIGHT"),pen="BORDER_THICK")
  
  createFreezePane(sheet=sheet,rowSplit = 3,colSplit = 3)
  
  macierz.rpn.istotnych=macierz.rpn
  macierz.rpn.istotnych[macierz.p>=alfa.do.pliku]=NA
  a=as.data.frame(macierz.rpn.istotnych)

  
  rows<-createRow(sheet=sheet, rowIndex = c(1:(nrow(macierz.rpn)+2)))
  setRowHeight(rows,30)
  setColumnWidth(sheet=sheet, colIndex = c(1:(ncol(macierz.rpn)+2)), 16)
  
  addDataFrame(a, sheet=sheet, startColumn=2, row.names=TRUE,rownamesStyle = cs4,colnamesStyle = cs4,startRow = 2)
  
  addMergedRegion(sheet=sheet,startRow = 3,endRow = (2+nrow(macierz.rpn)),startColumn=1,endColumn = 1)
  addMergedRegion(sheet=sheet,startRow = 1,endRow = 1,startColumn=3,endColumn = (2+ncol(macierz.rpn)))

  df.nazwa.cechy.w.kolumnach=data.frame(x1=rep(nazwa.cech.w.kolumnach,times=ncol(macierz.rpn)))
  df.nazwa.cechy.w.wierszach=data.frame(x1=rep(nazwa.cech.w.wierszach,times=nrow(macierz.rpn)))
  addDataFrame(df.nazwa.cechy.w.wierszach$x1, sheet=sheet, startColumn = 1, startRow = 3,row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs4))
  addDataFrame(df.nazwa.cechy.w.kolumnach$x1, sheet=sheet, startColumn = 3, startRow = 1,row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs4),byrow=TRUE)
  
  rows  <- getRows(sheet)
  cells <- getCells(rows) 
  values <- lapply(cells, getCellValue)
  
  klasy=floor(10*(abs(macierz.r)))
  klasy[macierz.r==1]=9
  
  for (i in c(3:9))
  {
    ktore.okna.naleza.do.klasy=which(klasy==i,arr.ind = TRUE)
    ile.z.danej.klasy=nrow(ktore.okna.naleza.do.klasy)

    if (ile.z.danej.klasy>0)
    {
      for (k in c(1:ile.z.danej.klasy))
      {
        if(!is.na(macierz.rpn.istotnych[ktore.okna.naleza.do.klasy[k,1],ktore.okna.naleza.do.klasy[k,2]]))
        {
          eval(parse(text=paste("setCellStyle(cell=cells$'",(ktore.okna.naleza.do.klasy[k,1]+2),".",(ktore.okna.naleza.do.klasy[k,2]+2),"',cellStyle=CellStyle(wb) + Fill(foregroundColor = paleta[i]) +Alignment(wrapText = TRUE)+Font(wb, isBold=TRUE))",sep="")))
          
        } # koniec if(!is.na(macierz.rpn.istotnych[ktore.okna.naleza.do.klasy[k,1],ktore.okna.naleza.do.klasy[k,2]]))
      } # koniec for (k in c(1:ile.z.danej.klasy))
    } # koniec if (ile.z.danej.klasy>0)

    
  } # koniec for (i in c(1:9))


} # koniec funkcji