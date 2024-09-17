package com.cosmetic.gg.authentication.repository;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.User;

@Repository
public interface AuthRepository extends CrudRepository<User, String>{
	User findByUsername(String username);
}
