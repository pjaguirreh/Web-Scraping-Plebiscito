remDr$findElement(using = "css", value = "#menu > ul > li:nth-child(2) > a")$clickElement()

webElem$sendKeysToElement(list("A"))

remDr$findElement(using = "css", value = "tr.nivelUno:nth-child(5) > td:nth-child(3) > small:nth-child(1) > span:nth-child(1)")$getElementText()
remDr$findElement(using = "css", value = "tr.nivelUno:nth-child(2) > td:nth-child(3) > small:nth-child(1) > span:nth-child(1)")$getElementText()
