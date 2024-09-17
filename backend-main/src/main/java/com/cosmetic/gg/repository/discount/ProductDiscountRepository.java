package com.cosmetic.gg.repository.discount;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.discount.ProductDiscount;

@Repository
public interface ProductDiscountRepository extends JpaRepository<ProductDiscount, String>{

	@Query(value = "SELECT * FROM product_discount t INNER JOIN product_item t2 ON t2.product_id=t.product_id " +
			"WHERE (t.discount_id=:discountId AND t2.id=:productItemId)"
			, nativeQuery = true)
	ProductDiscount findByDiscountAndProduct(@Param("productItemId") String productItemId, @Param("discountId") String discountId);
}
