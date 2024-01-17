use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};
use std::rc::Rc;

#[derive(Debug)]
struct Program {
    nodes: Vec<Node>,
    start: usize,
}

type Vars = HashMap<String, i32>;

#[derive(Debug)]
enum Node {
    Start,
    Halt,
    Assign(Vec<(String, Expr<i32>)>),
    Branch(Expr<bool>, usize, usize),
}

#[derive(Clone)]
enum Expr<Ret> {
    Func {
        str: String,
        func: Rc<dyn Fn(&Vars) -> Ret>,
    },
    Subs(Box<Expr<Ret>>, HashMap<String, Expr<i32>>),
}

impl<Ret> Debug for Expr<Ret> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Func { str, .. } => write!(f, "{}", str),
            Expr::Subs(inner, table) => {
                let mut inner = format!("{:?}", inner);
                for (name, expr) in table {
                    inner = inner.replace(name, &format!("{:?}", expr));
                }
                write!(f, "{}", inner)
            }
        }
    }
}

impl<Ret> Expr<Ret> {
    fn apply(&self, vars: &Vars) -> Ret {
        match self {
            Expr::Func { func, .. } => func(vars),
            Expr::Subs(inner, table) => {
                let mut vars = vars.clone();
                for (name, expr) in table {
                    vars.insert(name.clone(), expr.apply(&vars));
                }
                inner.apply(&vars)
            }
        }
    }
}

macro_rules! expr_to_rust_expr {
    ($vars:expr, $left:tt $op:tt $right:tt) => {
        expr_to_rust_expr!($vars, $left) $op expr_to_rust_expr!($vars, $right)
    };
    // This rule must come before the $lit:literal rule because the `-` in `-x`
    // counts as the start of a literal.
    ($vars:expr, $op:tt $right:tt) => {{
        $op expr_to_rust_expr!($vars, $right)
    }};
    // This rule must come before the $name:ident rule because `true` counts as
    // an identifier.
    ($vars:expr, $lit:literal) => {
        $lit
    };
    ($vars:expr, $name:ident) => {
        $vars.get(stringify!($name)).cloned().unwrap_or_default()
    };
    ($vars:expr, ( $($inner:tt)+ )) => {
        ( expr_to_rust_expr!($vars, $($inner)+) )
    };
    ($vars:expr, { var $name:expr }) => {
        $vars.get($name).cloned().unwrap_or_default()
    };
    ($vars:expr, { expr $expr:expr }) => {
        $expr.apply($vars)
    };
    /*
    ($vars:expr, { $expr:expr }) => {
        $expr
    };
    */
}

macro_rules! expr_to_string {
    // The order of the rules here must match the order of the rules in the
    // expr_to_rust_expr! macro.
    ($left:tt $op:tt $right:tt) => {{
        format!("{} {} {}",
            &expr_to_string!($left),
            stringify!($op),
            &expr_to_string!($right),
        )
    }};
    ($op:tt $right:tt) => {{
        format!("{}{}",
            stringify!($op),
            &expr_to_string!($right),
        )
    }};
    ($lit:literal) => {{
        stringify!($lit).to_string()
    }};
    ($name:ident) => {{
        stringify!($name).to_string()
    }};
    (( $($inner:tt)+ )) => {{
        format!("({})", &expr_to_string!($($inner)+))
    }};
    ({ var $name:expr }) => {{
        $name.clone()
    }};
    ({ expr $expr:expr }) => {
        format!("({:?})", $expr)
    };
    /*
    ({ $expr:expr }) => {
        $expr
    };
    */
}

macro_rules! expr {
    ($($expr:tt)+) => {
        Expr::Func {
            str: expr_to_string!($($expr)+),
            func: Rc::new(move | #[allow(unused_variables)] vars: &Vars| {
                (expr_to_rust_expr!(vars, $($expr)+)).into()
            }),
        }
    };
}

/// Syntax for creating a node:
/// node ::= start | halt | <assignment> | <branch> | <goto>
/// assignment ::= <name> := <expr>
/// branch ::= if (<expr>), <label>, <label>
/// goto ::= goto <label>
/// expr ::= <expr> <op> <expr> | <name> | <literal> | (<expr>)
macro_rules! node {
    (start) => { Node::Start};
    (halt) => { Node::Halt };
    ($name:ident := $($value:tt)+) => {
        Node::Assign(vec![
            (stringify!($name).to_string(), expr!($($value)+))
        ])
    };
    (if ( $($cond:tt)+ ), $t:expr, $f:expr) => {
        Node::Branch(expr!($($cond)+), $t, $f)
    };
    (goto $label:literal) => {
        node!(if (true), $label, $label)
    };
}

impl Node {
    pub fn run(&self, vars: &mut Vars, pc: &mut usize, stop: &mut bool) {
        match self {
            Node::Start => *pc += 1,
            Node::Halt => *stop = true,
            Node::Assign(assignments) => {
                let values = assignments
                    .iter()
                    .map(|(name, expr)| (name.clone(), expr.apply(vars)))
                    .collect::<Vec<_>>();
                for (name, value) in values {
                    vars.insert(name, value);
                }
                *pc += 1;
            }
            Node::Branch(cond, t, f) => {
                if cond.apply(vars) {
                    *pc = *t;
                } else {
                    *pc = *f;
                }
            }
        }
    }
}

type Path = Vec<usize>;

impl Program {
    pub fn new(nodes: impl Into<Vec<Node>>) -> Self {
        let nodes = nodes.into();
        let start = nodes.iter().position(|n| matches!(n, Node::Start)).unwrap();
        Self { nodes, start }
    }

    pub fn run(&self, vars: &mut Vars) -> Path {
        let mut pc = self.start;
        let mut stop = false;
        let mut path = vec![];
        while !stop {
            path.push(pc);
            self.nodes[pc].run(vars, &mut pc, &mut stop);
        }
        path
    }

    pub fn is_path_valid(&self, path: &Path) -> bool {
        let Some(mut last) = path.last() else {
            return false;
        };

        for (&label, &next_label) in path.iter().zip(path.iter().skip(1)) {
            match self.nodes.get(label) {
                None => return false,
                Some(Node::Start | Node::Halt | Node::Assign(..)) => {
                    if next_label != label + 1 {
                        return false;
                    }
                }
                Some(Node::Branch(_, t, f)) => {
                    if next_label != *t && next_label != *f {
                        return false;
                    }
                }
            }
        }
        true
    }

    pub fn get_transformations_and_reachabitily(
        &self,
        path: &Path,
    ) -> (Expr<bool>, HashMap<String, Expr<i32>>) {
        assert!(self.is_path_valid(path));

        let mut t = HashMap::new();
        let mut r = expr!(true);

        for current_path_index in (0..path.len()).rev() {
            match &self.nodes[path[current_path_index]] {
                // start and halt don't do anything
                Node::Start | Node::Halt => (),
                // t <- t[y <- e]
                // r <- r[y <- e]
                Node::Assign(assignments) => {
                    let assignments: HashMap<_, _> = assignments.iter().cloned().collect();
                    // Make sure every variable we are going to use and isn't
                    // in the table gets mapped by  t[name] |-> name
                    for name in assignments.keys() {
                        if !t.contains_key(name) {
                            let name = name.clone();
                            t.insert(name.clone(), expr!({ var & name }));
                        }
                    }
                    // Do t[name] <- t[name][y <- e]
                    // i.e. use `assignments` on every entry.
                    for expr in t.values_mut() {
                        *expr = Expr::Subs(expr.clone().into(), assignments.clone());
                    }
                    // Do r <- r[y <- e]
                    r = Expr::Subs(r.into(), assignments.clone());
                }
                // r <- r && (l_(m+1) = T ? B(x) : !B(x))
                Node::Branch(cond, t, f) => {
                    let t = *t;
                    let f = *f;
                    let cond = cond.clone();

                    // This is our `l_(m+1)`
                    let prev_node_label = path[current_path_index + 1];
                    let branch_taken_condition = if prev_node_label == t {
                        // `B(x)`
                        cond
                    } else {
                        assert_eq!(path[current_path_index + 1], f);
                        // `!B(x)`
                        expr!(!{ expr cond })
                    };
                    r = expr!({ expr branch_taken_condition } && { expr r });
                }
            }
        }
        (r, t)
    }
}

fn do_cli(program: &Program, vars: &Vars, path: &Path) {
    let original_vars = vars;
    let mut vars = original_vars.clone();

    let path_taken = program.run(&mut vars);
    let (r, t) = program.get_transformations_and_reachabitily(path);

    println!("The program:");
    for (i, node) in program.nodes.iter().enumerate() {
        println!("{: >2} | {:?}", i, node);
    }
    println!();
    println!(
        "Variable state at the start of the program: {:?}",
        original_vars
    );
    println!("Variable state at the end of the program: {:?}", vars);
    println!("Path taken: {:?}", path_taken);
    println!();
    println!("For the path: {:?}", path);
    println!("Reachability condition: {:?}", r);
    println!("Transformations: {:?}", t);
}

fn main() {
    // To see the interesting parts of this code, look at
    // `Program::get_transformations_and_reachabitily`.

    // This "language" is slightly different from "real" flow programs - branches
    // don't have 2 children, but have indexes for lines to jump to for true and
    // false cases. (Why? It was easier to implement.)
    let program = Program::new([
        node!(start),
        node!(x := input),
        node!(if (x >= 0), 3, 5),
        node!(x := x * x),
        node!(goto 6),
        node!(x := - (x * x)),
        node!(halt),
    ]);
    let path = vec![0, 1, 2, 5, 6];
    let vars = [("input".into(), 0)];
    let vars = vars.into_iter().collect();
    do_cli(&program, &vars, &path);
}
