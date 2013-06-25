##### set measure "2:%0.2VALUE %UNITS//angstroms"
   set defaultdistancelabel "2:%0.6VALUE %UNITS//angstroms"
   
   measure allconnected (Ag*) (Au*); show measurements
   measures delete
   
   measure allconnected (Au*) (Au*); show measurements
   measures delete
   
   measure allconnected (Ag*) (Ag*); show measurements
   measures delete
   
   
   
#NAGAG
grep -B1 Measurement */*.txt  | grep measurement | tr \[ \ | tr \] \  | grep "=  Ag" | grep ", Ag" | awk '{print $1, $2}' > NAGAG.txt
#NAGAU
grep -B1 Measurement */*.txt  | grep measurement | tr \[ \ | tr \] \  | grep "=  Ag" | grep ", Au" | awk '{print $1, $2}' > NAGAU.txt
#NAUAU
grep -B1 Measurement */*.txt  | grep measurement | tr \[ \ | tr \] \  | grep "=  Au" | grep ", Au" | awk '{print $1, $2}' > NAUAU.txt
   
   
for i in NA*.txt; do cat $i | tr \/ \  | awk '{print $2, $3}' | tee $i.2; mv $i $i.old; mv $i.2 $i; done
   
   
   
   AUAU=read.table("NAUAU.txt")
   AGAG=read.table("NAGAG.txt")
   AGAU=read.table("NAGAU.txt")
   
   colnames(AGAG)=c("Name","NAGAG")
   colnames(AGAU)=c("Name","NAGAU")
   colnames(AUAU)=c("Name","NAUAU")
   
   df2=merge(AGAG,AGAU,by="Name",all=TRUE)
   df3=merge(df2,AUAU,by="Name",all=TRUE)
   
   
   df3[is.na(df3)] <- 0

   df3$NAUAU[df3$NAUAU>0]=df3$NAUAU[df3$NAUAU>0]+1
   
   df3$NAGAU[df3$NAGAU>0]=df3$NAGAU[df3$NAGAU>0]+1
   
   df3$NAGAG[df3$NAGAG>0]=df3$NAGAG[df3$NAGAG>0]+1
   
   df3$NTOT=df3$NAUAU+df3$NAGAU+df3$NAGAG
   
   write.table(df3, file = "df3.txt", append = FALSE,row.names=T,quote=F,sep="\t")
   
   