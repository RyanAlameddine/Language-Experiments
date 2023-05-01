use hydroflow::tokio;
use hydroflow::util::{bind_udp_bytes, ipv4_resolve};
use hydroflow::hydroflow_syntax;

#[tokio::main]
async fn main() {
    let mut flow = hydroflow_syntax! {
        input = source_stdin() -> map(Result::unwrap) -> map(|x| x.parse::<i32>().unwrap());
        output = map(|x| x.to_string()) -> for_each(|x| println!("{}", x));
        

        /* All below is generated */
        // c0 = map(|x| x * 2);

        c2 = map(|x| x % 2 == 0);
        c5 = map(|x| x * 2);
        c6 = map(|x| x + 1);
        c3 = tee();
        c3[0] -> c5;
        c3[1] -> c6;
        c4 = cross_join::<'tick, 'tick>() -> map(|(x, y)| (x, y));
        c5 -> [0]c4;
        c6 -> [1]c4;
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
