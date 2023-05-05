use hydroflow::tokio;
use hydroflow::util::{bind_udp_bytes, ipv4_resolve};
use hydroflow::hydroflow_syntax;

#[tokio::main]
async fn main() {
    let mut flow = hydroflow_syntax! {
        input = source_stdin() -> map(Result::unwrap) -> map(|x| x.parse::<i32>().unwrap());
        output = for_each(|x| println!("{:?}", x));
        

        /* Paste generated code below */

        c2 = map(|x| x % 2 == 0);
        c5 = map(|x| (x, x));
        c8 = map(|x| x + 1);
        c9 = map(|x| x * 2);
        c6 = tee();
        c6[0] -> map(|(x, _)| x) -> c8;
        c6[1] -> map(|(_, x)| x) -> c9;
        c7 = join::<'tick, 'tick>() -> map(|(k, v)| v);
        c8 -> enumerate() -> [0]c7;
        c9 -> enumerate() -> [1]c7;
        c3 = c5;
        c5 -> c6;
        c4 = c7;
        c0 = tee();
        c0[0] -> c2;
        c0[1] -> c3;
        c1 = cross_join::<'tick, 'tick>() -> map(|(b, (o1, o2))| if b {o1} else {o2});
        c2 -> [0]c1;
        c4 -> [1]c1;
        
        input -> c0;
        c1 -> output;



    };

    flow.run_async().await;
}
