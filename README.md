# LINGI2369-Orthogonal-Range-Searching

Authors :
Massin Guillaume & Verhaeghe Hélène

The datasets used in the reader class are available on these links :
http://data.okfn.org/data/core/co2-fossil-global
http://data.okfn.org/data/amercader/car-fuel-and-emissions
https://www.maxmind.com/en/free-world-cities-database

To see examples of how to use the search methods, see the test classes or the reader.

The search is done on generic points. You can either use the standart Point class using several dimensions with the same types of values or define your own new class by extending the SpacePoint abstract class. For example, searches can be done on the class Town which handles (id, TownName, (Latitude,Longigude)) as (int, String, (Double,Double)).

Warning : the is currently a bug with the fractional cascading where some points are not reported in the in the results in some cases. 
