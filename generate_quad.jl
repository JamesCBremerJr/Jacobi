using FastGaussQuadrature

ntimes = 1
fid    = open("quad.dat","w")

i1 = 6
i2 = 24

if  i2 == 20
themax = 10^6
else
themax = 10^7
end

(xs,whts) = gaussjacobi(2^10,.5,.25);


######################
da = 0.25
db = 0.00


println(fid,da)
println(fid,db)
println(fid,i2-i1+1)


num      = i2-i1+1
times    = zeros(num)
dnus     = zeros(num)


for i=i1:i2

idx = i-i1+1
n   = 2^i
n   = max(101,n)
n   = min(themax,n)
@printf("n = %8.8d     ",n);


tic();
for j=1:ntimes
(xs,whts) = gaussjacobi(n,da,db);
end 
dtime = toq()/ntimes;

times[idx] = dtime;
dnus[idx]  = n;

@printf("  %15.3g\n",times[idx]);


println(fid,dnus[idx])
println(fid,times[idx])

# write 100 random whts to the disk
#idxs=rand(20:n-20,100);
idxs=rand(20:n-20,100);

for  j=1:100
println(fid,idxs[j])
println(fid,whts[idxs[j]])
end



end


#################################################


da =-0.49
db = 0.25


println(fid,da)
println(fid,db)
println(fid,i2-i1+1)


num      = i2-i1+1
times    = zeros(num)
dnus     = zeros(num)


for i=i1:i2

idx = i-i1+1
n   = 2^i
n   = max(101,n)
n   = min(themax,n)
@printf("n = %8.8d     ",n);


tic();
for j=1:ntimes
(xs,whts) = gaussjacobi(n,da,db);
end 
dtime = toq()/ntimes;

times[idx] = dtime;
dnus[idx]  = n;

@printf("  %15.3g\n",times[idx]);


println(fid,dnus[idx])
println(fid,times[idx])

# write 100 random whts to the disk
#idxs=rand(20:n-20,100);
idxs=rand(20:n-20,100);

for  j=1:100
println(fid,idxs[j])
println(fid,whts[idxs[j]])
end



end


#################################################




#################################################


da = 0.00
db =-0.40


println(fid,da)
println(fid,db)
println(fid,i2-i1+1)


num      = i2-i1+1
times    = zeros(num)
dnus     = zeros(num)


for i=i1:i2

idx = i-i1+1
n   = 2^i
n   = max(101,n)
n   = min(themax,n)
@printf("n = %8.8d     ",n);


tic();
for j=1:ntimes
(xs,whts) = gaussjacobi(n,da,db);
end 
dtime = toq()/ntimes;

times[idx] = dtime;
dnus[idx]  = n;

@printf("  %15.3g\n",times[idx]);


println(fid,dnus[idx])
println(fid,times[idx])

# write 100 random whts to the disk
#idxs=rand(20:n-20,100);
idxs=rand(20:n-20,100);

for  j=1:100
println(fid,idxs[j])
println(fid,whts[idxs[j]])
end



end


#################################################




close(fid)

