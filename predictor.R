predictor<-function(item1){
        library(dplyr)
        library(stringi)
        library(qdap)
        library(tm)
        library(stringr)
        #change the name so that they can match with Lasso model coefficient names
        item1$filt_brand<-item1$Brand
        names(item1)[9]<-paste0('filt_brand',item1$Brand)
        names(item1)[4]<-paste0('FirstCategory', item1$First.category)
        names(item1)[8]<-paste0('item_conditionid', item1$ItemCondition)
        names(item1)[2]<-'shipping'
        item1[,c(9)]<-1
        item1[,c(4)]<-1
        
        item1$DummyBrand<-ifelse(is.na(item1$Brand),0,1)
        item1$DummyItemDescription<-ifelse(is.na(item1$Item.description),0,1)
        
        names(item1)[10]<-paste0(names(item1)[10], item1[,c(10)])
        names(item1)[11]<-paste0(names(item1)[11], item1[,c(11)])
        
        #Tokenized item
        #words transformation for Corpros creation
        item1$item_description_processed<-tolower(item1$Item.description)
        item1$item_description_processed<-removePunctuation(item1$item_description_processed)
        item1$item_description_processed<-stripWhitespace(item1$item_description_processed)
        item1$item_description_processed<-replace_abbreviation(item1$item_description_processed)
        item1$item_description_processed<-replace_contraction(item1$item_description_processed)
        item1$item_description_processed<-replace_symbol(item1$item_description_processed)
        
        ###tokenize###
        toks <- tokens(item1$item_description_processed,
                       remove_punct = TRUE,
                       remove_separators = TRUE,
                       remove_symbols = TRUE)
        
        ###token selection###
        #remove stop words from toks
        nostop_toks <- tokens_select(toks, stopwords('en'), selection = 'remove')
        
        #try bigram
        bigram <- tokens_ngrams(nostop_toks, n = 2)
        
        dfm2=dfm(bigram)#2
        d_dfm=convert(dfm2,to="data.frame")
        
        item_df<-cbind(item1[,c(2,4,8:11)], d_dfm)
        
        match_df<-as.data.frame(matrix(data=0, nrow=1, ncol=length(trainng2)-1))
        names(match_df)<-names(trainng2)[2:ncol(trainng2)]
        
        for (i in 1:ncol(match_df)){
                
                match_df[,c(i)]<-ifelse(names(match_df)[i] %in% names(item_df),1,0)
                
        }
        
        
        for(i in 173:ncol(match_df)){
                if(class(match_df[,i])=='numeric'){match_df[,i]<-as.factor(match_df[,i])}
        }
        
        suggestedprice<-predict(lmgn2, match_df)
        suggestedprice<-exp(suggestedprice)-1
        return(suggestedprice)
}
