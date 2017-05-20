:- include('edges.pro').



adj(X, Y, W) :- edge(X, Y, W) ; edge(Y, X, W).


get_neighbours_dists(Visited, NDists) :-                 % Получаем все расстояние для соседних вершин.
    findall(D, (member([X, Dx], Visited),                % Выбираем из всех посещённых вершин
        adj(X, Y, W),                                    % Смежные вершины,
        \+ member([Y, Dy], Visited),                     % Которые ещё не были посещены.
        D is Dx + W),                                    % И складываем расстояния до них.
    NDists).


find_neighbour(Visited, CDist, Nearest) :-               % Находим рядом стоящие вершины для наименьшей.
    member([Y, Dy], Visited),                            % Берём те, которые посещены.
    D is CDist - Dy,                                     % Рассматриваемое расстояние берём как текущее — расстояние до этой вершины.
    adj(Y, Nearest, D),                                  % Находим все вершины с таким расстоянием (соседей) и скалывадем в Nearest.
    \+ member([Nearest, Dc], Visited).                   % Берём в Nearest только непосещённые вершины.


get_nearest(Visited, Nearest, CDist) :-                  % Функция для получения смежных вершин.
    get_neighbours_dists(Visited, NDists),               % Получаем список непосещённых соседей.
    min_list(NDists, CDist),                             % Выбираем соседа с минимальным расстоянием.
    find_neighbour(Visited, CDist, Nearest),             % Находим расстояния соседей этой вершины.
    !.


dijkstra(Visited, Finish, Dist) :- 
    member([Finish, Dist], Visited), 
    !.

dijkstra(Visited, Finish, Dist) :- 
    get_nearest(Visited, Nearest, CDist),                  % Выбираем вершины, смежные посещённым.
    dijkstra([[Nearest, CDist]|Visited], Finish, Dist),    % Запускаем дейкстру с новым списком посещённых вершин.
    !.


dist(Start, Finish, Dist) :- 
    dijkstra([[Start, 0]], Finish, Dist).
