//
//  tupleprint.h
//  vm
//
//  Created by Ovidiu Podisor on 01/30/19.
//  Copyright © 2019-2021 innodocs. All rights reserved.
//

#ifndef TUPLEPRINT_h
#define TUPLEPRINT_h

template <class... TArgs>
std::ostream& operator<<(std::ostream& os, std::tuple<TArgs...> const& t) {

  bool first = true;
  std::apply([&os, &first](auto&&... args) {
    auto print = [&] (auto&& val) {
      if (!first)
        os << ", ";
      (os << val);
      first = false;
    };
    (print(args), ...);
  }, t);

  return os;
}

#endif /* TUPLEPRINT_h */
