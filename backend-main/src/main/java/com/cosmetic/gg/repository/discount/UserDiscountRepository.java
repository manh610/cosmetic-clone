package com.cosmetic.gg.repository.discount;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.discount.UserDiscount;

@Repository
public interface UserDiscountRepository extends JpaRepository<UserDiscount, String>{

	@Query( value = "SELECT * FROM user_discount t WHERE t.discount_id=:id", nativeQuery = true)
	List<UserDiscount> findAllUSerByDiscount(@Param("id") String id);
	
	@Query(value = "SELECT * FROM user_discount t WHERE (t.discount_id=:discountId AND t.user_id=:userId)", nativeQuery = true)
	UserDiscount findByDiscountAndUser(@Param("userId") String userId, @Param("discountId") String discountId);
}
