use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};

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
    Assign(Vec<(String, Expr)>),
    Branch(Expr, usize, usize),
}

#[derive(Clone)]
enum Expr {
    Expr { str: String, func: fn(&Vars) -> i32 },
    And(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    Subs(Box<Expr>, HashMap<String, Expr>),
    Var(String),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Expr { str, .. } => write!(f, "{}", str),
            Expr::And(left, right) => write!(f, "({:?} && {:?})", left, right),
            Expr::Not(expr) => write!(f, "!({:?})", expr),
            Expr::Subs(inner, table) => {
                let mut inner = format!("{:?}", inner);
                for (name, expr) in table {
                    inner = inner.replace(name, &format!("{:?}", expr));
                }
                write!(f, "{}", inner)
            }
            Expr::Var(name) => write!(f, "{}", name),
        }
    }
}

impl Expr {
    fn apply(&self, vars: &Vars) -> i32 {
        match self {
            Expr::Expr { func, .. } => func(vars),
            Expr::And(left, right) => left.apply(vars) & right.apply(vars),
            Expr::Not(expr) => 1 - expr.apply(vars),
            Expr::Subs(inner, table) => {
                let mut vars = vars.clone();
                for (name, expr) in table {
                    vars.insert(name.clone(), expr.apply(&vars));
                }
                inner.apply(&vars)
            }
            Expr::Var(name) => vars.get(name).unwrap_or(&0).clone(),
        }
    }
}

macro_rules! expr_to_rust_expr {
    ($vars:expr, ( $($inner:tt)+ )) => {
        ( expr_to_rust_expr!($vars, $($inner)+) )
    };
    ($vars:expr, { var $name:expr }) => {
        $vars.get($name).unwrap_or(&0).clone()
    };
    ($vars:expr, { expr $expr:expr }) => {
        $expr.apply($vars)
    };
    ($vars:expr, { $expr:expr }) => {
        $expr
    };
    ($vars:expr, $left:tt $op:tt $right:tt) => {
        expr_to_rust_expr!($vars, $left) $op expr_to_rust_expr!($vars, $right)
    };
    ($vars:expr, $name:ident) => {
        $vars.get(stringify!($name)).unwrap_or(&0).clone()
    };
    ($vars:expr, $lit:literal) => {
        $lit
    };
}

macro_rules! expr {
    ($($expr:tt)+) => {
        Expr::Expr {
            str: stringify!($($expr)+).to_string(),
            func: |vars: &Vars| (expr_to_rust_expr!(vars, $($expr)+)).into(),
        }
    };
}

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
                if cond.apply(vars) != 0 {
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

    pub fn run(&self, vars: &mut Vars) {
        let mut pc = self.start;
        let mut stop = false;
        while !stop {
            self.nodes[pc].run(vars, &mut pc, &mut stop);
        }
    }

    pub fn get_transformations_and_reachabitily(&self, path: &Path) -> (Expr, HashMap<String, Expr>) {
        let mut t = HashMap::new();
        let mut r = expr!(true);

        for current_path_index in (0..path.len()).rev() {
            match &self.nodes[path[current_path_index]] {
                Node::Start => break,
                Node::Halt => (),
                Node::Assign(assignments) => {
                    for (name, expr) in assignments {
                        let current_expr = t.get(name).cloned().unwrap_or(Expr::Var(name.clone()));
                        let new_expr = Expr::Subs(
                            Box::new(current_expr),
                            vec![(name.clone(), expr.clone())].into_iter().collect(),
                        );
                        t.insert(name.clone(), new_expr);
                        r = Expr::Subs(
                            Box::new(r),
                            vec![(name.clone(), expr.clone())].into_iter().collect(),
                        );
                    }
                }
                Node::Branch(cond, t, f) => {
                    let t = *t;
                    let f = *f;
                    let k = if path[current_path_index + 1] == t {
                        cond.clone()
                    } else {
                        assert_eq!(path[current_path_index + 1], f);
                        Expr::Not(Box::new(cond.clone()))
                    };
                    r = Expr::And(Box::new(r), Box::new(k));
                }
            }
        }
        (r, t)
    }
}

fn main() {
    let program = Program::new([
        node!(start),
        node!(x := input),
        node!(if (x >= 0), 3, 5),
        node!(x := x * x),
        node!(goto 6),
        node!(x := 0 - (x * x)),
        node!(halt),
    ]);
    let path = vec![0, 1, 2, 5, 6];
    let mut vars: HashMap<_, _> = vec![("input".into(), 5)].into_iter().collect();
    let original_vars = vars.clone();
    program.run(&mut vars);
    println!("{:?}", program);
    println!(
        "Variable state at the start of the program: {:?}",
        original_vars
    );
    println!("Variable state at the end of the program: {:?}", vars);
    dbg!(program.get_transformations_and_reachabitily(&path));
}
