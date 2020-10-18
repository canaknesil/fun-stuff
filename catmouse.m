% Cat and mouse problem simulation
a = 5; % dummy

function dx = simX(t, x, y, s)

dx = s * (cos(t) - x) / sqrt((cos(t) - x)^2 + (sin(t) - y)^2);

end

function dy = simY(t, x, y, s)

dy = s * (sin(t) - y) / sqrt((cos(t) - x)^2 + (sin(t) - y)^2);

end


dt = 0.001;
t = 0 : dt : 2*pi;
s = 0.8;

mouse = exp(j * t);

catX(1) = 0;
catY(1) = 0;

i = 2;
for now = t
    catX(i) = simX(now, catX(i-1), catY(i-1), s) * dt + catX(i-1);
    catY(i) = simY(now, catX(i-1), catY(i-1), s) * dt + catY(i-1);
    i = i + 1;
end

%abs((catX + j * catY)')
plot(catX + j * catY)
hold
plot(mouse)


