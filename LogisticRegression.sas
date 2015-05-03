**read txt file into a SAS dataset;
data RAW;
  infile 'C:\Users\Jianghui\Desktop\BreastCancer\breast-cancer-wisconsin.data.txt' dlm=',' truncover; /*do not read to the next line*/
  input ID $ Thickness Size Shape Adhesion Epithelial Nuclei Bland Nucleoli Mitoses Class ;

  label Thickness = 'Clump Thickness'
        Size = 'Uniformity of Cell Size'
        Shape = 'uniformity of Cell Shape'
        Adhesion = 'Marginal Adhesion'
        Epithelial = 'Single Epithelial Cell Size'
        Nuclei = 'Bare Nuclei'
        Bland = 'Bland Chromatin'
        Nucleoli = 'Normal Nucleoli'
        ; 
run; 

data clean;
   set RAW;

   if Class = 4 then Class = 1;   *Malignant 241;
   else if Class = 2 then Class = 0;  *Benign 458;

   if ranuni(100) <= 0.7 then 
          do;
          train_test_ind = 'train';  *create training and validation datasets;
		  class_train = class;
		  end;
   else   do;
          train_test_ind = 'test';
          class_train = . ;
		  end;
run;

**check to make sure sample and 1/0 size  ;
proc freq data = clean;
  tables train_test_ind*class;
run;

data train;
  set clean;
  where train_test_ind = 'train';
  keep ID  Thickness Size Shape Adhesion Epithelial Nuclei Bland Nucleoli Mitoses Class;
run;

data test;
  set clean;
  where train_test_ind = 'test';
  keep ID  Thickness Size Shape Adhesion Epithelial Nuclei Bland Nucleoli Mitoses Class;
run;

proc export data = train dbms=csv
   outfile = "C:\Users\Jianghui\Desktop\BreastCancer\train.csv";
run;

proc export data = test dbms=csv
   outfile = "C:\Users\Jianghui\Desktop\BreastCancer\test.csv";
run;

proc freq data = train;
  tables Class*nuclei / missing;
run;

**check if missing and class variables match description ;
proc freq data = clean;
   tables Class Nuclei  Class*nuclei / missing;
run;

**logistic regression model since dependent variable is binary;
data clean;
  set clean;
  if nuclei ~= .;
run;

**check the correlations;
proc corr data = clean noprob out = outcorr ;
    var Thickness Size Shape Adhesion Epithelial Nuclei Bland Nucleoli Mitoses Class;
run;

proc logistic data = clean desc; /* predict 1 */

/*   class Mitoses;*/

   model Class_train = Thickness Size Shape Adhesion Epithelial Nuclei Bland Nucleoli Mitoses 
   Thickness*Size Thickness*Shape Thickness*Adhesion Thickness*Epithelial Thickness*Nuclei Thickness*Bland Thickness*Nucleoli Thickness*Mitoses 
   Size*Shape Size*Adhesion Size*Epithelial Size*Nuclei Size*Bland Size*Nucleoli Size*Mitoses 
   Shape*Adhesion Shape*Epithelial Shape*Nuclei Shape*Bland Shape*Nucleoli Shape*Mitoses
   Adhesion*Epithelial Adhesion*Nuclei Adhesion*Bland Adhesion*Nucleoli Adhesion*Mitoses
   Epithelial*Nuclei Epithelial*Bland Epithelial*Nucleoli Epithelial*Mitoses 
   Nuclei*Bland Nuclei*Nucleoli Nuclei*Mitoses
   Bland*Nucleoli Bland*Mitoses
   Nucleoli*Mitoses

   / selection = BACKWARD sls = 0.001; /*significant level to stay*/

   output out = scored p = p_hat;
 run;


**create listchart;
 data test;
    set scored;
	where train_test_ind = 'train';
run;

 **check VIF correlation;
proc reg data = test;
    model class = size Nuclei Nucleoli / tol vif collin;
run;

proc rank data = test out = ranky ties = low groups = 5;
   var p_hat;
   ranks r_score;
run;

proc means data = ranky noprint NWAY;
     class r_score;
	 var class;
	 output out = report (rename = (_freq_ = n))
		mean(class) = avg_class;
run;

**type I and type II error analysis;
data predict;
  set scored;
  if p_hat > 0.4 then target = 1;
  else target = 0;
run;

proc freq data = predict;
  tables train_test_ind*class*target;
run;

/*proc freq data = scored;*/
/*   tables p_hat;*/
/*run;*/

**calculate guide tree prediction;
data guide;
  set test;
  
  if Size <=2.5 or size = . then pred = 0;
  else 
     do;
     if Nuclei <=2.5 or Nuclei = . then 
	      do;
		  if size <= 3.5 then pred = 0;
		  else pred = 1;
		  end;
	 else pred = 1;
	 end;
run;

proc freq data = guide;
  tables class*pred;
run;
