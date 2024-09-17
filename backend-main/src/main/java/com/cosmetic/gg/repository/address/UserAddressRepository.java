package com.cosmetic.gg.repository.address;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.address.UserAddress;

@Repository
public interface UserAddressRepository extends JpaRepository<UserAddress, String>{

	@Query( value = "SELECT count(*) FROM user_address t INNER JOIN address t2 ON t2.id=t.address_id " +
			"WHERE (t.user_id=:id AND t2.status='ACTIVE') ", nativeQuery = true)
	Integer cntAddressByUser(@Param("id") String id);
}
