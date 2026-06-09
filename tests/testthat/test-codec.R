test_that("encode is a bijection onto 1:space_size", {
  codec <- make_state_codec(num_pins = 3, max_abs = 2)
  all_states <- expand.grid(rep(list(-2:2), 3))
  codes <- apply(all_states, 1, function(state) codec$encode(unname(state)))
  expect_setequal(codes, seq_len(codec$space_size))
})
