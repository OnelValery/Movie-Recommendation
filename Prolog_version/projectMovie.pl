:- dynamic user/3, movie/2.
% K
min_liked(10).
% R
liked_th(3.5).
% N
number_of_rec(20).


read_users(Filename) :-
    csv_read_file(Filename, Data), assert_users(Data).
assert_users([]).
assert_users([row(U,_,_,_) | Rows]) :- \+number(U),!, assert_users(Rows).
assert_users([row(U,M,Rating,_) | Rows]) :- number(U),\+user(U,_,_), liked_th(R), Rating>=R,!,assert(user(U,[M],[])), assert_users(Rows).
assert_users([row(U,M,Rating,_) | Rows]) :- number(U),\+user(U,_,_), liked_th(R), Rating<R,!,assert(user(U,[],[M])), assert_users(Rows).
assert_users([row(U,M,Rating,_) | Rows]) :- number(U), liked_th(R), Rating>=R, !, retract(user(U,Liked,NotLiked)), assert(user(U,[M|Liked],NotLiked)), assert_users(Rows).
assert_users([row(U,M,Rating,_) | Rows]) :- number(U), liked_th(R), Rating<R, !, retract(user(U,Liked,NotLiked)), assert(user(U,Liked,[M|NotLiked])), assert_users(Rows).

read_movies(Filename) :-
    csv_read_file(Filename, Rows), assert_movies(Rows).

assert_movies([]).
assert_movies([row(M,_,_) | Rows]) :- \+number(M),!, assert_movies(Rows).
assert_movies([row(M,Title,_) | Rows]) :- number(M),!, assert(movie(M,Title)), assert_movies(Rows).

display_first_n(_, 0) :- !.
display_first_n([], _) :- !.
display_first_n([H|T], N) :-
    writeln(H), 
    N1 is N - 1,
    display_first_n(T, N1).


% Similarity calculation using the Jaccard indx
similarity(U1, U2, Sim) :-
    % Retrieve liked and not liked movies of both users
    user(U1, L1, D1),
    user(U2, L2, D2),

    % Calculate intersections and unions
    intersection(L1, L2, LI),            % Liked intersection
    intersection(D1, D2, DI),            % Disliked intersection
    union(L1, L2, LU),                   % Liked union
    union(D1, D2, DU),                   % Disliked union
    union(LU, DU, AllUnion),             % Total union

    % Compute lengths
    length(LI, LenLI),
    length(DI, LenDI),
    length(AllUnion, LenAllUnion),

    % Jaccard similarity calculation
    (LenAllUnion > 0 -> Sim is (LenLI + LenDI) / LenAllUnion ; Sim = 0.0).

% Predicate to compute the score of a movie of a user
score(User, Movie, Score) :-
    % Find users who liked the movie
    findall(U, (user(U, Liked, _), member(Movie, Liked)), LikedUsers),
    
    % Ensure the movie has been liked by at least K users
    length(LikedUsers, LikedCount),
    min_liked(K), LikedCount >= K,

    % Calculate the sum of similarities between User and users who liked the movie
    findall(Sim, (member(U, LikedUsers), U \= User, similarity(User, U, Sim)), Similarities),
    sum_list(Similarities, Score).

% Predicate to calculate the probability that User will like Movie
prob(User, Movie, Prob) :-
    % Find users who liked the movie
    findall(U, (user(U, Liked, _), member(Movie, Liked)), LikedUsers),
    
    % Ensure that the movie has been liked by at least K users
    length(LikedUsers, LikedCount),
    min_liked(K),
    
    % If the movie has been liked by less than K users, set the probability to 0.0
    (   LikedCount < K -> 
        Prob = 0.0
    ;   % Otherwise, compute the score of the movie
        score(User, Movie, Score),
        
        % Compute the probability: score / number of users who liked the movie
        Prob is Score / LikedCount
    ).

%  This computes the probabilities of a list of movies of a  user.
prob_movies(User, Movies, Recs) :-
    findall((Title, Prob), 
            (member(MovieID, Movies), 
             movie(MovieID, Title), 
             prob(User, MovieID, Prob)), 
            Recs).

% This extracts the users in the list who liked a specific movie.
liked(Movie, Users, UserWhoLiked) :-
    findall(User, 
            (member(User, Users),
             user(User, Liked, _),        % Get the users liked movies list
             member(Movie, Liked)),       % Check weither the movie is in the liked list
            UserWhoLiked).

% This determines weither a user has seen a specific movie.
seen(User, Movie) :-
    user(User, Liked, NotLiked),  % Retrieve the liked and disliked movies lists
    (member(Movie, Liked) ; member(Movie, NotLiked)).  % Check weither the movie is in either list.

% This generates a list of movie recommendations of a user based on the computed probabilities.
recommendations(User) :-  
    setof(M,L^movie(M,L),Ms),  % generate list of all movie 
    prob_movies(User,Ms,Rec),  % compute probabilities  all movies    
    sort(2,@>=,Rec,Rec_Sorted), % sort by descending probabilities 
    number_of_rec(N),  
    display_first_n(Rec_Sorted,N). % display the result


init :- read_users("C:/Users/monel/Uottawa/4eAnnee/Hiver 2025/CSI2520/Project/Dataset/ml-latest-small/ratings.csv"), 
read_movies("C:/Users/monel/Uottawa/4eAnnee/Hiver 2025/CSI2520/Project/Dataset/ml-latest-small/movies.csv").
test(1):- similarity(33,88,S1), 291 is truncate(S1 * 10000),similarity(44,55,S2), 138 is truncate(S2 * 10000).
test(2):- prob(44,1080,P1), 122 is truncate(P1 * 10000), prob(44,1050,P2), 0 is truncate(P2).
test(3):- liked(1080, [28, 30, 32, 40, 45, 48, 49, 50], [28, 45, 50]).
test(4):- seen(32, 1080), \+seen(44, 1080).
test(5):- prob_movies(44,[1010, 1050, 1080, 2000],Rs), length(Rs,4), display(Rs).


                                   