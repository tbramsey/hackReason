:- use_module(library(clpfd)).
:- dynamic hasTakenTemp/1.
:- dynamic available_class/1.
hasTakenTemp().

% Facts about Bill

hates(_):- fail.
loves(_):- fail.
hasTaken(cs1200).
hasTaken(ecs1100).
hasTaken(core1).


% Facts about Sally
/*
hates(dr_johnson).
loves(dr_martin).
hasTaken(cs1200).
hasTaken(math2413).
hasTaken(elective1).
hasTaken(elective2).
hasTaken(phys2125).
hasTaken(phys2325).
hasTaken(phys2326).
hasTaken(cs2336).
hasTaken(math2414).
hasTaken(cs1337).
hasTaken(cs2305).
hasTaken(cs3377).
hasTaken(cs3305).
hasTaken(cs3340).
hasTaken(math2418).
hasTaken(core7).
hasTaken(core6).
hasTaken(core5).
hasTaken(core4).
hasTaken(core3).
hasTaken(core2).
hasTaken(core1).
*/


% Professor ratings
professor_rating(dr_johnson, 5).
professor_rating(dr_white, 3).
professor_rating(dr_richardson, 5).
professor_rating(dr_clark, 3).
professor_rating(dr_martin, 2).
professor_rating(dr_brown, 1).
professor_rating(dr_brown, 1).

% CS Degree Requirements
%requiredCourse(Class) :-
    %requiredCourseOption(Class, _).
requiredCourse(cs4485).
requiredCourse(cs4384).
requiredCourse(cs4349).
requiredCourse(cs4348).
requiredCourse(cs4347).
requiredCourse(cs4341).
requiredCourse(cs4337).
requiredCourse(ecs3390).
requiredCourse(cs3377).
requiredCourse(ecs3361).
requiredCourse(cs3354).
requiredCourse(cs3345).
requiredCourse(cs3341).
requiredCourse(cs3340).
requiredCourse(cs3162).
requiredCourse(cs2336).
requiredCourse(phys2326).
requiredCourse(phys2325).
requiredCourse(cs3305).
requiredCourse(math2418).
requiredCourse(cs2305).
requiredCourse(phys2125).
requiredCourse(cs1337).
requiredCourse(cs1200).
requiredCourse(ecs1100).
requiredCourse(csguided2).
requiredCourse(csguided1).
requiredCourse(elective4).
requiredCourse(elective3).
requiredCourse(elective2).
requiredCourse(elective1).
requiredCourse(core7).
requiredCourse(core6).
requiredCourse(core5).
requiredCourse(core4).
requiredCourse(core3).
requiredCourse(core2).
requiredCourse(core1).
requiredCourse(math2413).
requiredCourse(math2414).
% requiredCourseOption(math2413, math2417).
% requiredCourseOption(math2414, math2419).


% Prerequisites for courses
prereq(_, _).
prereq(cs2336, cs1337).
prereq(cs3340, cs1337).
prereq(cs3305, cs2305).
prereq(cs3340, cs2305).
prereq(phys2326, phys2325).
prereq(cs3341, cs2305).
prereq(cs3345, cs2305).
prereq(cs3345, cs2336).
prereq(cs3377, cs2336).
prereq(cs4341, phys2326).
prereq(cs4341, cs3340).
prereq(cs4337, cs3340).
prereq(cs4337, cs2305).
prereq(cs4337, cs2336).
prereq(cs3354, cs2305).
prereq(cs3354, cs2336).
prereq(cs4349, cs3345).
prereq(cs4349, cs3305).
prereq(cs3162, cs3354).
prereq(cs4348, cs3345).
prereq(cs4348, cs3377).
prereq(cs4348, cs3340).
prereq(cs4384, cs3305).
prereq(cs4347, cs3345).
prereq(cs4348, cs3354).
prereq(cs4348, cs3345).
prereq(cs4485, cs2336).
prereq(csguided1, cs3305).
prereq(csguided2, cs3305).
prereq(phys2325, math2413).
prereq(cs3305, math2414).
prereq(cs3341, math2414).
prereq(math2414, math2413).
prereq(math2418, math2413).
prereq(cs3345, core7).
prereq(ecs3390, core7).
prereq(ecs3361, core7).
prereq(cs3345, core6).
prereq(ecs3390, core6).
prereq(ecs3361, core6).
prereq(cs3345, core5).
prereq(ecs3390, core5).
prereq(ecs3361, core5).
prereq(cs4485, cs3345).
prereq(cs4337, cs3345).
prereq(core7, core4).
prereq(core6, core4).
prereq(core5, core4).
prereq(core4, core2).
prereq(core3, core2).
prereq(core4, core1).
prereq(core3, core1).
prereq(elective1, core4).
prereq(elective2, elective1).
prereq(elective3, elective1).
prereq(elective4, elective1).

% Classes with professors and class numbers
class(cs4485, dr_johnson, 4485.001).
class(cs4485, dr_richardson, 4485.002).
class(cs4485, dr_martin, 4485.003).
class(cs4485, dr_clark, 4485.004).
class(cs4384, dr_johnson, 4384.001).
class(cs4384, dr_richardson, 4384.002).
class(cs4384, dr_brown, 4384.003).
class(cs4349, dr_martin, 4349.001).
class(cs4349, dr_johnson, 4349.002).
class(cs4349, dr_white, 4349.003).
class(cs4348, dr_richardson, 4348.001).
class(cs4348, dr_clark, 4348.002).
class(cs4347, dr_johnson, 4347.001).
class(cs4347, dr_martin, 4347.002).
class(cs4347, dr_brown, 4347.003).
class(cs4341, dr_richardson, 4341.001).
class(cs4341, dr_white, 4341.002).
class(cs4337, dr_johnson, 4337.001).
class(cs4337, dr_clark, 4337.002).
class(ecs3390, dr_martin, 3390.001).
class(ecs3390, dr_richardson, 3390.002).
class(ecs3390, dr_brown, 3390.003).
class(cs3377, dr_johnson, 3377.001).
class(cs3377, dr_white, 3377.002).
class(ecs3361, dr_martin, 3361.001).
class(ecs3361, dr_clark, 3361.002).
class(cs3354, dr_johnson, 3354.001).
class(cs3354, dr_richardson, 3354.002).
class(cs3354, dr_brown, 3354.003).
class(cs3345, dr_martin, 3345.001).
class(cs3345, dr_johnson, 3345.002).
class(cs3345, dr_white, 3345.003).
class(cs3341, dr_richardson, 3341.001).
class(cs3341, dr_clark, 3341.002).
class(cs3340, dr_johnson, 3340.001).
class(cs3340, dr_martin, 3340.002).
class(cs3340, dr_brown, 3340.003).
class(cs3162, dr_richardson, 3162.001).
class(cs3162, dr_white, 3162.002).
class(cs2336, dr_johnson, 2336.001).
class(cs2336, dr_martin, 2336.002).
class(cs2336, dr_clark, 2336.003).
class(phys2326, dr_richardson, 2326.001).
class(phys2326, dr_brown, 2326.002).
class(phys2325, dr_johnson, 2325.001).
class(phys2325, dr_white, 2325.002).
class(cs3305, dr_martin, 3305.001).
class(cs3305, dr_johnson, 3305.002).
class(cs3305, dr_clark, 3305.003).
class(math2418, dr_richardson, 2418.001).
class(math2418, dr_martin, 2418.002).
class(math2418, dr_white, 2418.003).
class(cs2305, dr_johnson, 2305.001).
class(cs2305, dr_brown, 2305.002).
class(phys2125, dr_richardson, 2125.001).
class(phys2125, dr_clark, 2125.002).
class(cs1337, dr_martin, 1337.001).
class(cs1337, dr_johnson, 1337.002).
class(cs1337, dr_white, 1337.003).
class(cs1200, dr_johnson, 1200.102).
class(cs1200, dr_clark, 1200.103).
class(ecs1100, dr_martin, 1100.002).
class(ecs1100, dr_brown, 1100.003).
class(csguided2, dr_richardson, 4100.001).
class(csguided2, dr_white, 4100.002).
class(csguided1, dr_johnson, 4000.001).
class(csguided1, dr_clark, 4000.002).
class(elective4, dr_martin, 2300.001).
class(elective4, dr_richardson, 2300.002).
class(elective4, dr_brown, 2300.003).
class(elective3, dr_johnson, 2200.001).
class(elective3, dr_white, 2200.002).
class(elective2, dr_martin, 2100.001).
class(elective2, dr_clark, 2100.002).
class(elective1, dr_richardson, 2000.001).
class(elective1, dr_brown, 2000.002).
class(core7, dr_johnson, 1600.001).
class(core7, dr_white, 1600.002).
class(core6, dr_martin, 1500.001).
class(core6, dr_clark, 1500.002).
class(core5, dr_richardson, 1400.001).
class(core5, dr_johnson, 1400.002).
class(core5, dr_brown, 1400.003).
class(core4, dr_martin, 1300.001).
class(core4, dr_richardson, 1300.002).
class(core4, dr_white, 1300.003).
class(core3, dr_johnson, 1200.001).
class(core3, dr_clark, 1200.002).
class(core2, dr_martin, 1100.001).
class(core2, dr_brown, 1100.002).
class(core1, dr_richardson, 1000.001).
class(core1, dr_white, 1000.002).
class(math2413, dr_johnson, 2413.104).
class(math2413, dr_martin, 2413.105).
class(math2413, dr_clark, 2413.106).
class(math2414, dr_richardson, 2414.104).
class(math2414, dr_johnson, 2414.105).
class(math2414, dr_white, 2414.106).

classExists(ClassName) :-
    class(ClassName, _, _).

professorForClass(ClassName, Professor) :-
    class(ClassName, Professor, _).

classNumber(ClassName, ClassNumber) :-
    class(ClassName, _, ClassNumber).

update_available_classes :-
    retractall(available_class(_)), 
    forall(
        (requiredCourse(Class), met_prereq(Class)),
        assert(available_class(Class))
    ).

can_take(Class) :-
    available_class(Class).

met_prereq(Class) :-
    findall(Prereq, prereq(Class, Prereq), AllPrereqs),
    all_taken(AllPrereqs).

all_taken([]).
all_taken([Prereq|Rest]) :-
    (hasTaken(Prereq); hasTakenTemp(Prereq)),  % Prerequisite must be taken
    all_taken(Rest).

allClasses(Classes) :-
    findall(Class, (
        requiredCourse(Class),
        \+ hasTaken(Class),
        \+ hasTakenTemp(Class)
    ), Classes).

mark_as_taken(Classes) :-
    foreach(member(Class, Classes), (
        assertz(hasTakenTemp(Class))
    )).

unmark_as_taken(Classes) :-
    foreach(member(Class, Classes), (
        retractall(hasTakenTemp(Class))
    )).


nextSemester(Classes) :-
    update_available_classes,
    findall(Class, (
        requiredCourse(Class),
        \+ hasTaken(Class),
        \+ hasTakenTemp(Class),
        % can_take(Class)
        met_prereq(Class)
    ), AllClassesWithDuplicates),
    list_to_set(AllClassesWithDuplicates, AllClassesNoDuplicates),
    select_classes(AllClassesNoDuplicates, 5, Classes).

select_classes(_, 0, []) :- !.
select_classes([], _, []) :- !.  % Base case for when there are no more classes to select.
select_classes(AllClasses, N, [Class | Rest]) :-
    N > 0,
    select(Class, AllClasses, RemainingClasses),
    N1 is N - 1,
    select_classes(RemainingClasses, N1, Rest).

all_classes_taken :-
    \+ (requiredCourse(Class), \+ hasTaken(Class), \+ hasTakenTemp(Class)).

plan_all_semesters(Plan) :-
    plan_all_semesters_helper([], Plan),
    unmark_all_taken.

plan_all_semesters_with_professors(PlanWithProfessors) :-
    plan_all_semesters(Plan),
    maplist(pick_professor_for_semester, Plan, PlanWithProfessors),
    format_plan_by_semester(PlanWithProfessors).

format_plan_by_semester([]).
format_plan_by_semester([Semester | Rest]) :-
    write('Semester:'), nl,
    format_classes(Semester),
    nl,
    format_plan_by_semester(Rest).

format_classes([]).
format_classes([class(Class, Professor, ClassNumber) | Rest]) :-
    format('  ~w (~w, ~w)~n', [Class, Professor, ClassNumber]),
    format_classes(Rest).

% Match classes in the plan to their professors
pick_professor([], []).  % Base case: empty plan
pick_professor([Semester | Rest], [SemesterWithProfessors | RestWithProfessors]) :-
    pick_professor_for_semester(Semester, SemesterWithProfessors),
    pick_professor(Rest, RestWithProfessors).

% Pick professors for a semester
pick_professor_for_semester([], []).
pick_professor_for_semester([Class | RestClasses], [class(Class, Professor, ClassNumber) | RestWithProfessors]) :-
    findall(
        prof(Professor, ClassNumber),
        (class(Class, Professor, ClassNumber), \+ hates(Professor)),
        SuitableProfessors
    ),
    prioritize_professor(SuitableProfessors, Professor, ClassNumber),
    pick_professor_for_semester(RestClasses, RestWithProfessors).

% Prioritize professors
prioritize_professor([], unknown_professor, unknown_class_number).
prioritize_professor(SuitableProfessors, Professor, ClassNumber) :-
    % First, pick a professor the student loves
    member(prof(LovedProfessor, LovedClassNumber), SuitableProfessors),
    loves(LovedProfessor),
    !,
    Professor = LovedProfessor,
    ClassNumber = LovedClassNumber.
prioritize_professor(SuitableProfessors, Professor, ClassNumber) :-
    % Fallback: Pick the professor with the highest rating
    find_highest_rated(SuitableProfessors, Professor, ClassNumber).

% Find the professor with the highest rating
find_highest_rated(SuitableProfessors, BestProfessor, BestClassNumber) :-
    findall(
        Rating-Professor-ClassNumber,
        (member(prof(Professor, ClassNumber), SuitableProfessors), professor_rating(Professor, Rating)),
        RatedProfessors
    ),
    max_member(_-BestProfessor-BestClassNumber, RatedProfessors).
find_highest_rated([prof(DefaultProfessor, DefaultClassNumber) | _], DefaultProfessor, DefaultClassNumber).
plan_all_semesters_helper(Acc, Plan) :-
    \+ all_classes_taken,
    nextSemester(Classes),
    mark_as_taken(Classes),
    update_available_classes,
    plan_all_semesters_helper([Classes | Acc], Plan).

plan_all_semesters_helper(Plan, Plan) :-
    all_classes_taken.

unmark_all_taken :-
    retractall(hasTakenTemp(_)).