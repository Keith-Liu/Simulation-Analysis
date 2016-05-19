#Can Cheng 998266707
#Keith Liu

#the main idea of this R-code is to simulate the Single Server Queue 
#we do not need a GEL to handle the order of processing since we are using two arrays:
#arry1:arrival_time, keeps track of the arrival time of each packet.
#arry2:departure_time, keeps track of what time the packet departures.
#in order to achive the same effect as GEL, we uses two "pointers" in these two array.
#we first compare wether the arrival_time[a] is less than departure_time[b], if it is ture, then 
#we know that the arrival_time[a],(i.e pakcekt# a) should process before the departure events, thus
#it is a arrival events. Same thing apply for arrival_time[a] < departure[b], which is a departure
#event

#we do not need a queue for this R-code, since we know which one should go first and once we need 
#to put the arriving packege to queue, we can just update and delay the depature time for that 
#particular packet to inter_package_time+departure_time for the previous package.
 

rate_arrival=0.01  #set arrival rate
rate_process=1	#set process rate
num_package=100
MAXBUFFER=1
drop=0
busy=0
N=10 #number of hosts

generate_packet_size <- function(){
	size <- runif(1,64,1518)
	return (size)
}#function to generate the size of a packet

pick_destination <- function(num_dst,source){
	destination <- sample(1:num_dst,1)
	while (destination==source){
		destination <-sample(1:num_dst,1)
	}
	return (destination)
}#function to ramdomly pick a destination, since it can not pick
 #from itself, so we exclude it by a while loop, num_dst is the total number of host
 #source is the source host(itself)
	

negative_exponenetially_distributed_time <- function(rate){
rand_x <- runif(1,0,1)
y=(-1/rate)* log(1-rand_x)
return (y)
}#function to create a randome exponetially time

#now we pre create a package for arrival_time, we know all the time about
#when the next package is going to arrive
arrival_time <- matrix(list(),nrow=num_package,ncol=N) 
for (count in 1:N){
	for (i in 1:num_package){
		if(i==1){
			arrival_time[[i,count]]=0+negative_exponenetially_distributed_time(rate_arrival)
		}
		else{
			arrival_time[[i,count]]=arrival_time[[i-1,count]]+negative_exponenetially_distributed_time(rate_arrival)
		}
	}
}#we generate a two dimesional array to store all the information of arrival_time for each host
 #the rows are the packet number and the columes are the host number,same for the below

departure_time <- matrix(list(),nrow=num_package,ncol=N) 
for (count in 1:N){
	for (i in 1:num_package){
		if (i ==1){
			departure_time[[i,count]]=arrival_time[[i,count]]+negative_exponenetially_distributed_time(rate_process)
		}
		else{
			departure_time[[i,count]]=0
		}
	}#initialize the departure_time, we pre build the idea arrival time, but this will be updated 
 	#in the following code as we put them into buffer
}
#-------------------------------------------------------
trials=50#number of trails we want
a=c(1:N)
b=c(1:N)

for(i in 1:N){
	a[i]=2
	b[i]=1
}

length=c(1:N) #keeps track of how many packages in the server, we start with a=2, so there is already 
         	  #one pakage in it
for(i in 1:N){
	length[i]=0
}


for(count in 1:N){
	for (i in 1:trials){
		if (arrival_time[[a[count],count]] < departure_time[[b[count],count]]){ #this means it is a arrival event
			length[count]=length[count]+1
			if(length[count] <=1){ #this means we only have one packet in the server and 
					    #server is free now
				departure_time[[a[count],count]]=arrival_time[[a[count],count]]+negative_exponenetially_distributed_time(rate_process)
				a[count]=a[count]+1
			}
			else{ #this means the server is not free, length >1
				busy=busy+1
				departure_time[[a[count],count]]=departure_time[[a[count]-1,count]]+negative_exponenetially_distributed_time(rate_process)
				a[count]=a[count]+1
			}
		}#if statement
		else{ #this means a departure event
			length[count]=length[count]-1 #we decrease the number of pakcage in the server since we got one leaving
			departure_time[[a[count],count]]=arrival_time[[a[count],count]]+negative_exponenetially_distributed_time(rate_process)
			b[count]=b[count]+1
		}#else
	}#for loop for trials
}#for loop for N
#-------------------------------------------------V2
token_arrival_time=c(1:N)
for (i in 1:N){
	token_arrival_time[i]=100;
}

dst_number <- matrix(list(),nrow=num_package,ncol=N)
for(count in 1:N){
	for (i in 1:num_package){
		dst_number[[i,count]]=pick_destination(N,count)
	}
} #now, we generate a matrix that store the information of the destination of each package in each host


pkg_size <- matrix(list(),nrow=num_package,ncol=N)
for(count in 1:N){
	for (i in 1:num_package){
		pkg_size[[i,count]]=generate_packet_size()
	}
}

#---------------------------------------V3
w_count=1
time_use_to_dst <- matrix(list(),nrow=num_package,ncol=N) 
for(i in 1:N){
	for(count in 1:num_package){
		time_use_to_dst[[count,i]]=0
	}
}

process_delay_to_dst <- matrix(list(),nrow=num_package,ncol=N) 
for(i in 1:N){
	for(count in 1:num_package){
		process_delay_to_dst[[count,i]]=0
	}
}

propagation_delay_to_dst <- matrix(list(),nrow=num_package,ncol=N) 
for(i in 1:N){
	for(count in 1:num_package){
		propagation_delay_to_dst[[count,i]]=0
	}
}

throughput <- matrix(list(),nrow=num_package,ncol=N) #generate a matrix to store information about the throughput
for(i in 1:N){
	for(count in 1:num_package){
		throughput[[count,i]]=0
	}
}

host_distance <- matrix(list(),nrow=num_package,ncol=N)
for(i in 1:N){
	for(count in 1:num_package){
		host_distance[[count,i]]=0
	}
}
#store the information of distance between hosts for a pakage

for (pos_token in 1:N){
	while(token_arrival_time[pos_token] >= arrival_time[[w_count,pos_token]]){
		host_distance[[w_count,pos_token]]=abs(dst_number[[w_count,pos_token]]-pos_token)
		process_delay_to_dst[[w_count,pos_token]]=host_distance[[w_count,pos_token]]*(pkg_size[[w_count,pos_token]]*8/100) #we calculate the delay
														 #byte=8 bit and 1Mbps=100 bit per microsecond
		propagation_delay_to_dst[[w_count,pos_token]]=host_distance[[w_count,pos_token]]*10
		time_use_to_dst[[w_count,pos_token]]=propagation_delay_to_dst[[w_count,pos_token]]+process_delay_to_dst[[w_count,pos_token]]
		throughput[[w_count,pos_token]]=(pkg_size[[w_count,pos_token]])/time_use_to_dst[[w_count,pos_token]]
		if(w_count<100){
			w_count=w_count+1
		}			
	}
	w_count=1
}

sum=c(1:N)
for (i in 1:N){
	sum[i]=0
}

check=1
average_throughput=c(1:N)
for (i in 1:N){
	average_throughput[i]=0
}

for (host in 1:N){
	while(throughput[[check,host]]!=0){
		sum[host]=sum[host]+throughput[[check,host]]	
		check=check+1
	}
	average_throughput[host]=sum[host]/check
	check=1
}

sum_throughput=0
total_average_throughput=0
for (i in 1:N){
	sum_throughput=average_throughput[i]+sum_throughput
}
total_average_throughput=rate_arrival*10*sum_throughput/N

delay_time_sum=c(1:N)
for(i in 1:N){
	delay_time_sum[i]=0
}

for (host in 1:N){
	while(time_use_to_dst[[check,host]]!=0){
		delay_time_sum[host]=delay_time_sum[host]+time_use_to_dst[[check,host]]	
		check=check+1
	}
	check=1
}

sum_delay=0
total_average_delay=0
for (i in 1:N){
	sum_delay=delay_time_sum[i]+sum_delay
}
total_average_delay=sum_delay/N
total_average_throughput
total_average_delay
