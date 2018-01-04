[y,fs] = wavread('google1.wav');   
[y2,fs] = wavread('google2.wav');   
[y3,fs] = wavread('google3.wav');   
[y4,fs] = wavread('google4.wav');   
[y5,fs] = wavread('facebook1.wav');   
[y6,fs] = wavread('facebook2.wav');   
[y7,fs] = wavread('facebook3.wav');   
[y8,fs] = wavread('facebook4.wav');   



y = y(:,1);
dt = 1/fs;
t = 0:dt:(length(y)*dt)-dt;

y2 = y2(:,1);
dt = 1/fs;
t2 = 0:dt:(length(y2)*dt)-dt;

y3 = y3(:,1);
dt = 1/fs;
t3 = 0:dt:(length(y3)*dt)-dt;

y4 = y4(:,1);
dt = 1/fs;
t4 = 0:dt:(length(y4)*dt)-dt;

y5 = y5(:,1);
dt = 1/fs;
t5 = 0:dt:(length(y5)*dt)-dt;

y6 = y6(:,1);
dt = 1/fs;
t6 = 0:dt:(length(y6)*dt)-dt;

y7 = y7(:,1);
dt = 1/fs;
t7 = 0:dt:(length(y7)*dt)-dt;

y8 = y8(:,1);
dt = 1/fs;
t8 = 0:dt:(length(y8)*dt)-dt;
    
    subplot(1,4,1)
    plot(t,y); xlabel('Seconds'); ylabel('Amplitude');
    
    subplot(1,4,2)
    plot(t2,y2); xlabel('Seconds'); ylabel('Amplitude');
    
    subplot(1,4,3)
    plot(t3,y3); xlabel('Seconds'); ylabel('Amplitude');
    
    subplot(1,4,4)
    plot(t4,y4); xlabel('Seconds'); ylabel('Amplitude');
    
    figure;
        
    subplot(1,4,1)
    plot(t5,y5); xlabel('Seconds'); ylabel('Amplitude');
    
    subplot(1,4,2)
    plot(t6,y6); xlabel('Seconds'); ylabel('Amplitude');
    
    subplot(1,4,3)
    plot(t7,y7); xlabel('Seconds'); ylabel('Amplitude');
    
    subplot(1,4,4)
    plot(t8,y8); xlabel('Seconds'); ylabel('Amplitude');
