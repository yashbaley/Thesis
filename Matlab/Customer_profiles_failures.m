% see line no. 464,663,
tic
%% Reading excel file
clc; clear;
filename= 'SimulatedData_4000.csv';
sheet = 1;
NCustomers = 4000; %6 lakh customers
% [num, text, raw] = xlsread(filename,sheet) ;

%%
%%Generating ages
age = normrnd(35,15,[NCustomers,1]);
for i = 1:NCustomers
    if age(i)< 18
        age(i) = 18;
    elseif age(i) > 60
        age(i) = 60;
    end   
end
% xlswrite(filename,age,sheet,'C3'); 
%%
%% Generating gender
Gender_column = rand(NCustomers,1);
Gender = zeros(NCustomers,2);

for i = 1: NCustomers
    if(Gender_column(i) < 0.98) 
        Gender(i,1) = 1;
        
    elseif (Gender_column(i) >0.99)
        Gender(i,2) = 1;
        
    end
end  
% xlswrite(filename,Gender,sheet,'D3');

%%
Marital_status_Column = rand(NCustomers,1);
Marital_status = zeros(NCustomers,5);
for i = 1:NCustomers
   if(Marital_status_Column(i)<0.3)
       Marital_status(i,1) = 1;
   elseif(Marital_status_Column(i)>0.3 && Marital_status_Column(i)<0.35)
       Marital_status(i,2) = 1;
   elseif(Marital_status_Column(i)>0.35 && Marital_status_Column(i)<0.9)
       Marital_status(i,3) = 1;   
   elseif(Marital_status_Column(i)>0.9 && Marital_status_Column(i)<0.95)
       Marital_status(i,4) = 1;
   elseif(Marital_status_Column(i)>0.95 && Marital_status_Column(i)<1)
       Marital_status(i,5) = 1; 
   end 
end 
% xlswrite(filename,Marital_status,sheet,'F3');
%%
Position_in_family_Column = rand(NCustomers,1);
Position_in_family = zeros(NCustomers,3);
for i = 1:NCustomers 
   if(Position_in_family_Column(i)<0.25)
      Position_in_family(i,1) = 1;
   elseif(Position_in_family_Column(i)>0.25 && Position_in_family_Column(i)<0.75)
       Position_in_family(i,2) = 1;
   elseif(Position_in_family_Column(i)>0.75 && Position_in_family_Column(i)<1)
       Position_in_family(i,3) = 1;   
   end 
end 
% xlswrite(filename,Position_in_family,sheet,'K3');

%% Occupation generation
Occupation_Column = rand(NCustomers,1);
Occupation = zeros(NCustomers,6);
for i = 1:NCustomers
   if(Occupation_Column(i)<0.2)
       Occupation(i,1) = 1;
   elseif(Occupation_Column(i)>0.2 && Occupation_Column(i)<0.4)
       Occupation(i,2) = 1;
   elseif(Occupation_Column(i)>0.4 && Occupation_Column(i)<0.45)
       Occupation(i,3) = 1;   
   elseif(Occupation_Column(i)>0.45 && Occupation_Column(i)<0.60)
       Occupation(i,4) = 1;
   elseif(Occupation_Column(i)>0.60 && Occupation_Column(i)<0.80)
       Occupation(i,5) = 1; 
   elseif(Occupation_Column(i)>0.80 && Occupation_Column(i)<1)
       Occupation(i,6) = 1; 
   end 
end 
% xlswrite(filename,Occupation,sheet,'N3');
%%
%%Education
Education_Column = rand(NCustomers,1);
Education = zeros(NCustomers,5);
for i = 1:NCustomers
   if(Education_Column(i)<0.2)
       Education(i,1) = 1;
   elseif(Education_Column(i)>0.2 && Education_Column(i)<0.6)
       Education(i,2) = 1;
   elseif(Education_Column(i)>0.6 && Education_Column(i)<0.85)
       Education(i,3) = 1;   
   elseif(Education_Column(i)>0.85 && Education_Column(i)<0.95)
       Education(i,4) = 1;
   elseif(Education_Column(i)>0.95 && Education_Column(i)<1)
       Education(i,5) = 1; 
   end 
end 
% xlswrite(filename,Education,sheet,'T3');
%%
%%Affiliation
Affiliation_Column = rand(NCustomers,1);
Affiliation = zeros(NCustomers,3);
for i = 1:NCustomers 
   if(Affiliation_Column(i)<0.8)
      Affiliation(i,1) = 1;
   elseif(Affiliation_Column(i)>0.8 && Affiliation_Column(i)<0.9)
       Affiliation(i,2) = 1;
   elseif(Affiliation_Column(i)>0.9 && Affiliation_Column(i)<1)
       Affiliation(i,3) = 1;   
   end 
end 
% xlswrite(filename,Affiliation,sheet,'Y3');

%% 
%%Location
Location_Column = rand(NCustomers,1);
Location = zeros(NCustomers,3);
for i = 1:NCustomers 
   if(Location_Column(i)<0.5)
      Location(i,1) = 1;
   elseif(Location_Column(i)>0.5 && Location_Column(i)<0.7)
       Location(i,2) = 1;
   elseif(Location_Column(i)>0.7 && Location_Column(i)<1)
       Location(i,3) = 1;   
   end 
end 

% xlswrite(filename,Location,sheet,'AB3');

%%
%%Purpose
Purpose_Column = rand(NCustomers,1);
Purpose = zeros(NCustomers,3);
for i = 1:NCustomers 
   if(Purpose_Column(i)<0.3)
      Purpose(i,1) = 1;
   elseif(Purpose_Column(i)>0.3 && Purpose_Column(i)<0.8)
       Purpose(i,2) = 1;
   elseif(Purpose_Column(i)>0.8 && Purpose_Column(i)<1)
       Purpose(i,3) = 1;   
   end 
end 

% xlswrite(filename,Purpose,sheet,'AE3');

%%
%%Experience
%[num, text, raw] = xlsread(filename,sheet) ;
Experience_Column = normrnd(4,1,[NCustomers,1]);
Experience = zeros(NCustomers,1);
for i=1:NCustomers
    if(age(i,1)-Experience_Column(i,1)>18 && Experience_Column(i,1)>0)
        Experience(i,1) =  Experience_Column (i,1);
        
    end
end
% xlswrite(filename,Experience,sheet,'AH3');
%%
%%Weight
Weight = normrnd(80,10,[NCustomers,1]);
for i=1:NCustomers
    if(Weight(i)<40) 
        Weight(i)= 40;
    elseif(Weight(i)>120)
        Weight(i) = 110;
    end
end    
% xlswrite(filename,Weight,sheet,'AI3');
%%  
%%
%%Height
Height = normrnd(1.75,0.5,[NCustomers,1]);
for i=1:NCustomers
    if(Height(i)<1.5) 
        Height(i)= 1.5;
    elseif(Height(i)>2)
        Height(i) = 2;
    end
end    
% xlswrite(filename,Height,sheet,'AJ3');
%%  
%%MHabits
MHabits_Column = rand(NCustomers,1);
MHabits = zeros(NCustomers,2);
for i = 1:NCustomers 
   if(MHabits_Column(i)<0.4)
      MHabits(i,1) = 1;
   elseif(MHabits_Column(i)>0.4 )
       MHabits(i,2) = 1;
   end 
end 
% xlswrite(filename,MHabits,sheet,'AK3');
%%
%%Attraction
Attraction_Column = rand(NCustomers,1);
Attraction = zeros(NCustomers,2);
for i = 1:NCustomers 
   if(Attraction_Column(i)<0.6)
      Attraction(i,1) = 1;
   elseif(Attraction_Column(i)>0.4 )
       Attraction(i,2) = 1;
   end 
end 
% xlswrite(filename,Attraction,sheet,'AM3');
%%
%%
%%Distance
Distance = normrnd(8,4,[NCustomers,1]);
for i=1:NCustomers
    if(Distance(i)<0) 
        Distance(i)= 0;
    end
end    
% xlswrite(filename,Distance,sheet,'AO3');
%%
%%Duration
Duration = normrnd(1,0.5,[NCustomers,1]);
for i=1:NCustomers
    if(Duration(i)<0) 
        Duration(i)= 0;
   
    end
end    
% xlswrite(filename,Duration,sheet,'AP3');
%%
%%
%%Discipline
Discipline = normrnd(7,2,[NCustomers,1]);
for i=1:NCustomers
    if(Discipline(i)<0) 
        Discipline(i)= 0;
    elseif(Discipline(i)>10) 
        Discipline(i)= 10;
   
   
    end
end    
% xlswrite(filename,Discipline,sheet,'AQ3');
%%
%%Pillion
Pillion_Column = rand(NCustomers,1);
Pillion = zeros(NCustomers,2);
for i = 1:NCustomers 
   if(Pillion_Column(i)<0.3)
      Pillion(i,1) = 1;
   elseif(Pillion_Column(i)>0.3)
       Pillion(i,2) = 1;
   end 
end 
% xlswrite(filename,Pillion,sheet,'AR3');
%%
%%RHabits
RHabits_Column = rand(NCustomers,1);
RHabits = zeros(NCustomers,3);
for i = 1:NCustomers 
   if(RHabits_Column(i)<0.2)
      RHabits(i,1) = 1;
   elseif(RHabits_Column(i)>0.2 && RHabits_Column(i)<0.6)
       RHabits(i,2) = 1;
   elseif(RHabits_Column(i)>0.6 && RHabits_Column(i)<1)
       RHabits(i,3) = 1;   
   end 
end 

% xlswrite(filename,RHabits,sheet,'AT3');
%%
%%
%%Complaint
Complaint = normrnd(7,2,[NCustomers,1]);
for i=1:NCustomers
    if(Complaint(i)<0) 
        Complaint(i)= 0;
    elseif(Complaint(i)>10) 
        Complaint(i)= 10;
    end
end    
% xlswrite(filename,Complaint,sheet,'AW3');
%%
%%Outlook
Outlook_Column = rand(NCustomers,1);
Outlook = zeros(NCustomers,2);
for i = 1:NCustomers 
   if(Outlook_Column(i)<0.7)
      Outlook(i,1) = 1;
   elseif(Outlook_Column(i)>0.3)
       Outlook(i,2) = 1;
   end 
end 
% xlswrite(filename,Outlook,sheet,'AX3');
%%
%%Income
Income_Column = rand(NCustomers,1);
Income = zeros(NCustomers,3);
for i = 1:NCustomers 
   if(Income_Column(i)<0.6)
      Income(i,1) = 1;
   elseif(Income_Column(i)>0.6 && Income_Column(i)<0.95)
       Income(i,2) = 1;
   elseif(Income_Column(i)>0.95 && Income_Column(i)<1)
       Income(i,3) = 1;   
   end 
end 
% xlswrite(filename,Income,sheet,'AZ3');

%%
%%Religion
Religion_Column = rand(NCustomers,1);
Religion = zeros(NCustomers,4);
for i = 1:NCustomers 
   if(Religion_Column(i)<0.3)
      Religion(i,1) = 1;
   elseif(Religion_Column(i)>0.3 && Religion_Column(i)<0.4)
       Religion(i,2) = 1;
   elseif(Religion_Column(i)>0.4 && Religion_Column(i)<0.8)
       Religion(i,3) = 1;   
   elseif(Religion_Column(i)>0.8 && Religion_Column(i)<1)
       Religion(i,4) = 1;       
   end 
end 

% xlswrite(filename,Religion,sheet,'BC3');

%%
%%House
House_Column = rand(NCustomers,1);
House = zeros(NCustomers,2);
for i = 1:NCustomers 
   if(House_Column(i)<0.6)
      House(i,1) = 1;
   elseif(House_Column(i)>0.6)
       House(i,2) = 1;
   end 
end 
% xlswrite(filename,House,sheet,'BG3');
%%
%%House
% House_Column = rand(NCustomers,1);
% House = zeros(NCustomers,3);
% for i = 1:NCustomers 
%    if(House_Column(i)<0.6)
%       House(i,1) = 1;
%    elseif(House_Column(i)>0.6)
%        House(i,2) = 1;
%    end 
% end 
% xlswrite(filename,House,sheet,'BG3');

%%
%%Cars
Cars_Column = rand(NCustomers,1);
Cars = zeros(NCustomers,3);
for i = 1:NCustomers 
   if(Cars_Column(i)<0.6)
      Cars(i,1) = 1;
   elseif(Cars_Column(i)>0.6 && Cars_Column(i)<0.8)
       Cars(i,2) = 1;
   elseif(Cars_Column(i)>0.8 && Cars_Column(i)<1)
       Cars(i,3) = 1;   
   end 
end 

% xlswrite(filename,Cars,sheet,'BI3');
%%
Location_Section_Column = rand(NCustomers,1);
Location_Section = zeros(NCustomers,5);
for i = 1:NCustomers
   if(Location_Section_Column(i)<0.25)
       Location_Section(i,1) = 1;
   elseif(Location_Section_Column(i)>0.25 && Location_Section_Column(i)<0.35)
       Location_Section(i,2) = 1;
   elseif(Location_Section_Column(i)>0.35 && Location_Section_Column(i)<0.45)
       Location_Section(i,3) = 1;   
   elseif(Location_Section_Column(i)>0.45 && Location_Section_Column(i)<0.75)
       Location_Section(i,4) = 1;
   elseif(Location_Section_Column(i)>0.75 && Location_Section_Column(i)<1)
       Location_Section(i,5) = 1; 
   end 
end 
% xlswrite(filename,Location_Section,sheet,'BL3');

%%
%%Bikes
Bikes_Column = rand(NCustomers,1);
Bikes = zeros(NCustomers,2);
for i = 1:NCustomers 
   if(Bikes_Column(i)<0.6)
      Bikes(i,1) = 1;
   elseif(Bikes_Column(i)>0.6)
       Bikes(i,2) = 1;
   end 
end 
% xlswrite(filename,Bikes,sheet,'BQ3');
%%
%%Family
Family_Column = rand(NCustomers,1);
Family = zeros(NCustomers,3);
for i = 1:NCustomers 
   if(Family_Column(i)<0.3)
      Family(i,1) = 1;
   elseif(Family_Column(i)>0.3 && Family_Column(i)<0.6)
       Family(i,2) = 1;
   elseif(Family_Column(i)>0.6 && Family_Column(i)<1)
       Family(i,3) = 1;   
   end 
end 
% xlswrite(filename,Family,sheet,'BS3');

%% Generating Failure Possibility Score
FailureScoreMatrix = zeros(NCustomers,5);
%Column 1- Experience
%Column 2- Usage
%Column 3- Terrain
%Column 4- Maintanance Habit 
%Column 5- Riding Discipline
%Scale of 5 used for all five columns, 5 being highest failure...
%possibility
for i=1:NCustomers
    % For Experience in years
    if(Experience(i)<=0.5)
        FailureScoreMatrix(i,1) = 5;
    elseif(Experience(i)<=1.5 && Experience(i)>0.5)
        FailureScoreMatrix(i,1) = 4;
    elseif(Experience(i)<=2.5 && Experience(i)>1.5)
        FailureScoreMatrix(i,1) = 3;
    elseif(Experience(i)<=3.5 && Experience(i)>2.5)
        FailureScoreMatrix(i,1) = 2;
    elseif(Experience(i)>3.5)
        FailureScoreMatrix(i,1) = 1;
    end
    %For Usage(Distance in km/day)
    if(Distance(i)<=3)
        FailureScoreMatrix(i,2) = 1;
    elseif(Distance(i)<=5 && Distance(i)>3)
        FailureScoreMatrix(i,2) = 2;
    elseif(Distance(i)<=8 && Distance(i)>5)
        FailureScoreMatrix(i,2) = 3;
    elseif(Distance(i)<=11 && Distance(i)>8)
        FailureScoreMatrix(i,2) = 4;
    elseif(Distance(i)>11)
        FailureScoreMatrix(i,2) = 5;
    end
    
    %For Terrain
    if(Location(i,1)==1)
        FailureScoreMatrix(i,3) = 1;
    elseif(Location(i,2)==1)
        FailureScoreMatrix(i,3) = 3;
    elseif(Location(i,3)==1)
        FailureScoreMatrix(i,3) = 5;        
    end
    
    %For Maintenance Habits
    if(MHabits(i,1)==1)
        FailureScoreMatrix(i,4) = 1;
    elseif(MHabits(i,2)==1)
        FailureScoreMatrix(i,4) = 5;
    end    
    
    %For Riding Discipline
%     FailureScoreMatrix(i,5) = round(Discipline(i)/2,0);
    FailureScoreMatrix(i,5) = round(Discipline(i)/2);

end

% Categorising into A,B,C,D 
A = zeros(NCustomers,1);
B = zeros(NCustomers,1);
C = zeros(NCustomers,1);
D = zeros(NCustomers,1);
Category1=A;

SumFailureScoreMatrix = sum(FailureScoreMatrix,2);
for i = 1:NCustomers
    if(SumFailureScoreMatrix(i)<=7)
        A(i) = 1;
        Category1(i) = 1;
    elseif(SumFailureScoreMatrix(i)<=13 && SumFailureScoreMatrix(i)>7)
        B(i) = 1;
        Category1(i) = 2;
    elseif(SumFailureScoreMatrix(i)<=18 && SumFailureScoreMatrix(i)>13)
        Category1(i) = 3;
    elseif(SumFailureScoreMatrix(i)<=25 && SumFailureScoreMatrix(i)>18)
        Category1(i) = 4;
    end
end
% Category1 = Category1';

% xlswrite(filename,Category1,sheet,'BV3');

%% Generating Probability Matrix (Each column represents a type of failure
%and its value is the corresponding probability
Pa = [0 0.01 0.05 0.13  0.23 0.37 0.69];
Pb = [0 0.018 0.048 0.14 0.24 0.36 0.67 1];
Pc = [0 0.012 0.051 0.12 0.22 0.35 0.70 1];
Pd = [0 0.015 0.052 0.125 0.24 0.36 0.69 1];
RandomCol = zeros(NCustomers,1);
NMonths = 24;
Failure = zeros(NCustomers,NMonths);
% for i = 1:NCustomers
%     Generating a row of random numbers, each belonging to a month
%     RandomCol = rand(1,NMonths);
%     for j = 1:NMonths
%        if(A(i))
%            if(RandomCol(j)<0.35)  %Failure prob = 10%
%                Failure(i,j) = 1; 
%            end
%        elseif(B(i))
%            if(RandomCol(j)<0.38) %Failure prob = 12%
%                Failure(i,j) = 1;
%            end
%        elseif(C(i))
%            if(RandomCol(j)<0.42) %Failure prob = 14%
%                Failure(i,j) = 1;
%            end 
%        elseif(D(i))
%            if(RandomCol(j)<0.47) %Failure prob = 18% 
%                Failure(i,j) = 1;
%            end  
%        end    
%     end
% end    

%% asdf

beta_A = 2;
beta_B = 2;
beta_C = 2;
beta_D = 2;
eta_A = 100;
eta_B = 80;
eta_C = 50;
eta_D = 30;
for i = 1:NCustomers
    Time = 0;
    if(A(i))
        while(Time <= NMonths  )
            mttf = wblrnd(eta_A,beta_A);
            Time = Time + mttf/30;
            Time = round(Time);
            if(Time <24 && Time > 0 )
            Failure(i,Time) = 1;
            end
        end
    end
     if(B(i))
        while(Time <= 24  )
            mttf = wblrnd(eta_B,beta_B);
            Time = Time + mttf/30;
            Time = round(Time);
            if(Time <24 && Time > 0)
            Failure(i,Time) = 1;
            end
        end
     end
     if(C(i))
        while(Time <= 24 )
            mttf = wblrnd(eta_C,beta_C);
            Time = Time + mttf/30;
            Time = round(Time);
            if(Time <24 && Time > 0)
            Failure(i,Time) = 1;
            end
        end
     end
    if(D(i))
        while(Time <= 24  )
            mttf = wblrnd(eta_D,beta_D);
            Time = Time + mttf/30;
            Time = round(Time);
            if(Time <24 && Time > 0)
            Failure(i,Time) = 1;
            end
        end
    end
end

    


%% asdfa

for i=1:NCustomers
   for j=1:NMonths
       if(Failure(i,j)==1)
           Ftype = rand;
           if(A(i))
               for k=1:size(Pa,2)-1
                   if(Ftype>Pa(1,k) && Ftype<=Pa(1,k+1))
                       Failure(i,j) = k;
                   end 
               end
           end
           if(B(i))
               for k=1:size(Pb,2)-1
                   if(Ftype>Pb(1,k) && Ftype<=Pb(1,k+1))
                       Failure(i,j) = k;
                   end 
               end
           end
           if(C(i))
               for k=1:size(Pc,2)-1
                   if(Ftype>Pc(1,k) && Ftype<=Pc(1,k+1))
                       Failure(i,j) = k;
                   end 
               end
           end
           if(D(i))
               for k=1:size(Pd,2)-1
                   if(Ftype>Pd(1,k) && Ftype<=Pd(1,k+1))
                       Failure(i,j) = k;
                   end 
               end
           end
       end
   end
end
% xlswrite(filename,Failure,sheet,'BW3');

%% Checking frequency of each failure
FailureCount1 = 0;
FailureCount2 = 0;
FailureCount3 = 0;
FailureCount4 = 0;
FailureCount5 = 0;
FailureCount6 = 0;
FailureCount7 = 0;

for i=1:NCustomers
    for j=1:24
        if(Failure(i,j)==1)
            FailureCount1 = FailureCount1 +1;
        elseif(Failure(i,j)==2)
            FailureCount2 = FailureCount2 +1;
        elseif(Failure(i,j)==3)
            FailureCount3 = FailureCount3 +1;
        elseif(Failure(i,j)==4)
            FailureCount4 = FailureCount4 +1;
        elseif(Failure(i,j)==5)
            FailureCount5 = FailureCount5 +1;
        elseif(Failure(i,j)==6)
            FailureCount6 = FailureCount6 +1;
        elseif(Failure(i,j)==7)
            FailureCount7 = FailureCount7 +1;
        end
    end
end

%% Money spent on each failure
%Money spent by Customer for each failure, each column represents cost for
%corresponding failure number

%CustomerMoney = [CM1 CM2 CM3 CM4 CM5 CM6 CM7];
CustomerMoney = [7000 300 500 400 300 200 100]; %Placing random values just fro example

%Money spent by Service Provider for each failure, each column represents cost for
%corresponding failure number
ServiceMoney = [-4000 -3400 -2000 -1000 -500 -400 -200]; %Placing random values just for example

%Money spent by Manufacturer for each failure, each column represents cost for
%corresponding failure number
%ManufacturerMoney = [MM1 MM2 MM3 MM4 MM5 MM6 MM7];
ManufacturerMoney = [7000 300 3500 700 900 1200 200]; %Placing random values just for example

%% EMI Cash flow for customers
% Assuming 20% people take bike on EMI

RandRow = rand(NCustomers,1);
Loan = RandRow<=0.2;

% Assuming EMI = 7350 for bike price 1,50,000 on a ROI = 8.75% for 24 months
EMI = 7350;
EMICash = Loan*EMI;
EMICashFlow=repmat(EMICash,1,24);
% 
% for i=1:24
%     EMICashFlow(:,i) = EMICash(:,1); 
% end

%% Cost Matrix creation
CustomerCostMatrix = zeros(NCustomers,24) + EMICashFlow; %Adding EMI to the Cash flow of Cost to Customer
ManufacturerCostMatrix = zeros(NCustomers,24) - EMICashFlow; %subtracting EMI to the Cash flow of Cost to Manufacturer
ServiceProviderCostMatrix = zeros(NCustomers,24);

for i = 1:NCustomers
    for j = 1:24
        if Failure(i,j) ~= 0
        CustomerCostMatrix(i,j) = CustomerMoney(1,Failure(i,j));
        ManufacturerCostMatrix(i,j) = ManufacturerMoney(1,Failure(i,j));
        ServiceProviderCostMatrix(i,j) = ServiceMoney(1,Failure(i,j));      
        end
    end
end
xlswrite('Cost_Matrix.xlsx',CustomerCostMatrix,1,'B2');
xlswrite('Cost_Matrix.xlsx',ServiceProviderCostMatrix,2,'B2');
xlswrite('Cost_Matrix.xlsx',ManufacturerCostMatrix,3,'B2');



%% Finding Time value of money for sevice provider and manufacturer
%rate of interest is 'rate' %
% rate = 0.04;
% ServiceProviderTotal = 0;
% ManufacturerTotal = 0;
% for i = 1:NCustomers
%     for j = 1:NMonths
%         ServiceProviderTotal = ServiceProviderTotal + ServiceProviderCostMatrix(i,j)*(1+rate)^(NMonths-j);
%         ManufacturerTotal = ManufacturerTotal + ManufacturerCostMatrix(i,j)*(1+rate)^(NMonths-j);
%     end
% end
% CPV = [];
% CPV(1:NCustomers,1:15) = rand(NCustomers,15);
% sumMatrix = sum(CPV,2);
% for i=1:size(CPV,1)
%     CPV(i,:) = CPV(i,:)/sumMatrix(i);
% end
% %xlswrite(filename,CPV,10,'B2');
% % %Code completion message
% %[y,Fs] = audioread('tune.mp3');
% % sound(y,Fs)

%---- writing to datafile
dataToWrite=cat(2,age,Gender,Marital_status,Position_in_family,Occupation,Education,Affiliation,Location,Purpose,Experience,Weight,Height,MHabits,Attraction,Distance,Duration,Discipline,Pillion,RHabits,Complaint,Outlook,Income,Religion,House,Cars,Location_Section,Bikes,Family,Category1,Failure);
xlswrite(filename,dataToWrite,sheet,'C3'); 
toc     