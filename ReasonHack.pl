:- dynamic hasTakenTemp/1.
:- dynamic available_class/1.


% Facts about Bill
hasTaken(cs1200).
hasTaken(ecs1100).
hasTakenTemp().

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
prereq(cs3162, cs3345).
prereq(cs3162, cs3354).
prereq(cs4348, cs3345).
prereq(cs4348, cs3377).
prereq(cs4348, cs3340).
prereq(cs4384, cs3305).
prereq(cs4347, cs3345).
prereq(cs4348, cs3354).
prereq(cs4348, cs3345).
prereq(cs4485, cs2336).
prereq(csguided1, cs4341).
prereq(csguided2, cs4341).
prereq(phys2325, math2414).
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
prereq(core7, core4).
prereq(core6, core4).
prereq(core5, core4).
prereq(core4, core1).
prereq(core3, core1).
prereq(elective1, core4).
prereq(elective2, elective1).
prereq(elective3, elective1).
prereq(elective4, elective1).

% Classes with professors and class numbers
class(cs1200, dr_johnson, 1200.102).
class(ecs1100, dr_johnson, 1100.002).
class(math2413, dr_johnson, 241.104).

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
    forall(member(Class, Classes), assertz(hasTakenTemp(Class))).

unmark_as_taken(Classes) :-
    forall(member(Class, Classes), retractall(hasTakenTemp(Class))).

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