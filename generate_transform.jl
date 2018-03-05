using FastTransforms

da =-0.25
db = 0.00

ntimes = 1

x = randn(2^5);
y = cjt(x,da,db);
y = icjt(x,da,db);


# now time the code

i1       = 3
i2       = 24

themin   = 10

if  i2 == 20
themax = 10^6
elseif  i2 == 24
themax = 10^7
else
themax = 10^8
end



dd       = 2
num      = i2-i1+1
times    = zeros(num)
times2   = zeros(num)
dnus     = zeros(num)

for i=i1:i2
idx = i-i1+1

n   = round(Int,2^i)
n   = max(themin,n)
n   = min(themax,n);

@printf("n = %8.8d     ",n);

x = randn(n);
y = zeros(n);

# time forward transform;  average over ntimes trys
tic(); 
for j=1:ntimes
y = cjt(x,da,db);
end
times[idx] = toq()/ntimes; 
dnus[idx]  = n;



# time backward transform;  average over ntimes trys
tic(); 
for j=1:ntimes
y = icjt(x,da,db);
end

times2[idx] = toq()/ntimes; 
dnus[idx]   = n;


@printf("%12.4e seconds",times[idx])
@printf("   %12.4e seconds\n",times2[idx])

end




# write the times out to disk

fid = open("transform.dat","w")
println(fid,da)
println(fid,db)
println(fid,i2-i1+1)
for i=i1:i2
idx = i-i1+1
println(fid,dnus[idx])
println(fid,times[idx])
println(fid,times2[idx])
end 


###############################################################


da = 0.25
db =-0.40

ntimes = 1

x = randn(2^5);
y = cjt(x,da,db);
y = icjt(x,da,db);


# now time the code


dd       = 2
num      = i2-i1+1
times    = zeros(num)
times2   = zeros(num)
dnus     = zeros(num)

for i=i1:i2
idx = i-i1+1

n   = round(Int,2^i)
n   = max(themin,n)
n   = min(themax,n);

@printf("n = %8.8d     ",n);

x = randn(n);
y = zeros(n);

# time forward transform;  average over ntimes trys
#p = plan_cjt(x,da,db);
tic(); 
for j=1:ntimes
#y = p*x; 
y = cjt(x,da,db);
end
times[idx] = toq()/ntimes; 
dnus[idx]  = n;



# time backward transform;  average over ntimes trys
#p = plan_icjt(x,da,db);
tic(); 
for j=1:ntimes
#y = p*x; 
y = icjt(x,da,db);
end

times2[idx] = toq()/ntimes; 
dnus[idx]   = n;


@printf("%12.4e seconds",times[idx])
@printf("   %12.4e seconds\n",times2[idx])

end




# write the times out to disk

println(fid,da)
println(fid,db)
println(fid,i2-i1+1)
for i=i1:i2
idx = i-i1+1
println(fid,dnus[idx])
println(fid,times[idx])
println(fid,times2[idx])
end 

close(fid)

