README
================
Alex G
2/5/2021

``` r
#read in data for bridges from New York
NYbridges = read.csv(url('https://www.fhwa.dot.gov/bridge/nbi/2019/delimited/NY19.txt'), 
    sep=',', quote=NULL, comment='', header=T)
#select data needed for data table
NYbridges = select(NYbridges, STRUCTURE_NUMBER_008, YEAR_BUILT_027, PLACE_CODE_004, DECK_COND_058, SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061, CULVERT_COND_062, INVENTORY_RATING_066, BASE_HWY_NETWORK_012, STRUCTURE_FLARED_035)
#renaming each column in the datatable to make clearer
colnames(NYbridges) = c("Structure_Number", "Year_Built", "FIPS_Code", "Deck_Condition", "Superstructure_Condition", "Substructure_Condition", "Channel_Condition", "Culverts_Condition", "Inventory_Rating", "Base_Highway_Network", "Flares")

#interesting plot looking at the Superstructure condition versus the year built
ggplot(subset(NYbridges,!(Superstructure_Condition == "N")), aes(x = Superstructure_Condition, y = Year_Built))+
  geom_jitter(shape = 1, col = "blue", size = .3)+
  ggtitle("Bridges: Superstructure Condition vs. Year Built")+
  xlab("Superstructure Condition (low = bad, high = good)")+
  ylab("Year Built")
```

![](README_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
ggplot(subset(NYbridges,!(Substructure_Condition == "N")), aes(x = Substructure_Condition, y = Year_Built))+
  geom_jitter(shape = 1, col = "red", size = .3)+
  ggtitle("Bridges: Substructure(s) Condition vs. Year Built")+
  xlab("Substructure(s) Condition (low = bad, high = good)")+
  ylab("Year Built")
```

![](README_files/figure-markdown_github/unnamed-chunk-1-2.png)

<https://github.com/alexanderg10/433test.git>
