cropPrint <- print(data.frame(event = cropDamageSummary$event, 
                              cropDamage_USD = format(cropDamageSummary$cropDamage,
                                                      big.mark = ",")))
propertyPrint <- print(data.frame(event = propertyDamageSummary$event, 
                                  propertyDamage_USD = format(propertyDamageSummary$propertyDamage,
                                                              big.mark = ",")))
damagesPrint <- print(data.frame(event = damageSummary$event, 
                                 damages_USD = format(damageSummary$damage, 
                                                      big.mark = ",")))
injuryPrint <- print(data.frame(event = injurySummary$event, 
                                injuries = format(injurySummary$injuries, 
                                                  big.mark = ",")))
fatalityPrint <- print(data.frame(event = fatalitySummary$event, 
                                  fatalities = format(fatalitySummary$fatalities, 
                                                      big.mark = ",")))
casualityPrint<- print(data.frame(event = casualitySummary$event, 
                                  casualities = format(casualitySummary$casualities, 
                                                       big.mark = ",")))


injuryLast <- slice(injuryPrint, 1:10)
fatalityLast <- slice(fatalityPrint, 1:10)
casualityLast <- slice(casualityPrint, 1:10)
cropLLast <- slice(cropPrint, 1:10)
propertyLast <- slice(propertyPrint, 1:10)
damagesLast <- slice(damagesPrint, 1:10)

damageSum <- summarize(group_by(stormDamage, fixedEvent), 
                       allCrop = sum(cropDmgD),
                       allProperty = sum(propDmgD),
                       allDamage = sum(cropDmgD + propDmgD))