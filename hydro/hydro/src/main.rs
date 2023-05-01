use hydroflow::tokio;
use hydroflow::util::{bind_udp_bytes, ipv4_resolve};
use hydroflow::hydroflow_syntax;

#[tokio::main]
async fn main() {
    let mut flow = hydroflow_syntax! {
        input = source_stdin() -> map(Result::unwrap) -> map(|x| x.parse::<i32>().unwrap());
        output = for_each(|x| println!("{:?}", x));
        

        /* All below is generated */

        c2 = map(|x| (x, x));
c9 = map(|x| x % 2 == 0);
c12 = map(|x| x * 2);
c13 = map(|x| x + 1);
c10 = tee();
c10[0] -> c12;
c10[1] -> c13;
c11 = cross_join::<'tick, 'tick>() -> map(|(x, y)| (x, y));
c12 -> [0]c11;
c13 -> [1]c11;
c7 = tee();
c7[0] -> c9;
c7[1] -> c10;
c8 = cross_join::<'tick, 'tick>() -> map(|(b, (o1, o2))| if b {o1} else {o2});
c9 -> [0]c8;
c11 -> [1]c8;
c16 = map(|x| x % 2 == 0);
c19 = map(|x| x * 2);
c20 = map(|x| x + 1);
c17 = tee();
c17[0] -> c19;
c17[1] -> c20;
c18 = cross_join::<'tick, 'tick>() -> map(|(x, y)| (x, y));
c19 -> [0]c18;
c20 -> [1]c18;
c14 = tee();
c14[0] -> c16;
c14[1] -> c17;
c15 = cross_join::<'tick, 'tick>() -> map(|(b, (o1, o2))| if b {o1} else {o2});
c16 -> [0]c15;
c18 -> [1]c15;
c5 = c7;
c8 -> c14;
c6 = c15;
c23 = map(|x| x % 2 == 0);
c26 = map(|x| x * 2);
c27 = map(|x| x + 1);
c24 = tee();
c24[0] -> c26;
c24[1] -> c27;
c25 = cross_join::<'tick, 'tick>() -> map(|(x, y)| (x, y));
c26 -> [0]c25;
c27 -> [1]c25;
c21 = tee();
c21[0] -> c23;
c21[1] -> c24;
c22 = cross_join::<'tick, 'tick>() -> map(|(b, (o1, o2))| if b {o1} else {o2});
c23 -> [0]c22;
c25 -> [1]c22;
c3 = tee();
c3[0] -> map(|(x, _)| x) -> c5;
c3[1] -> map(|(_, x)| x) -> c21;
c4 = join::<'tick, 'tick>() -> map(|(k, v)| v);
c6 -> enumerate() -> [0]c4;
c22 -> enumerate() -> [1]c4;
c0 = c2;
c2 -> c3;
c1 = c4;

input -> c0;
c1 -> output;



    };

    flow.run_async().await;
}
