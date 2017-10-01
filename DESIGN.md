

# red-lights

### Principle
Optimize traffic lights schedule for Paris

MAYBE USE AN EXISTING ENGINE/GAME, or make a realistic virtual world instead of Paris

1. Get a good model of the traffic and schedule
2. Develop traffic unaware strategies that are better (metric to determine, probably median/average travel time)
3. Try traffic aware (cameras at traffic lights) strategies

### A good model
1. Get traffic lights data from OpenData (https://opendata.paris.fr/explore/dataset/signalisation-tricolore)
2. Link it to roads data from OpenstreetMap (see c_json_parser project)
3. Try to make vehicles travel on it and visualize that

### Strategies
1. Find a way to make travel strategies, maybe using an API (the behavior is more or less what Google Maps or Ways says), or maybe c_json_parser
2. Use a/many performance metrics
3. 


### Thoughts

maybe design as streams on roads, getting more or less dense -impacting speed- depending on the place and time, instead of individual vehicles (maybe faster and easier to get a good model)

maybe around 200 000 vehicles travelling inside Paris at any time
if we calculate the way to take before hand :
  200 000 * 200 nodes (?) = 40 000 000 integers in memory = 160 Mo
if we want to have 10 iterations per second :
  200 000 * 10 = 2 000 000 per second, so we only have 0.5 microsecond by vehicle by second
  for a simple loop incremeting an integer, we can do 2 000 000 000 per second (1000 times more)

make all cars start with a certain delay inbetween them when the traffic lights go green

maybe have a grid of all cars, to know when collisions would occur (to slow down and accelerate appropriately)
10km * 10km
10 000m * 10 000m = 100 000 000 pixels (of 1 byte, could be 1 bit) = 100 Mo : OK for a 1mx1m grid


o = node

    o                o
     #             #
      #          #
       #       #
light > o    o   <- light
         # #
         o   <- center junction
        #
       o   <- continue
      #
     #
    #
   o
