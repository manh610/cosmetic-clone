package com.cosmetic.gg.repository.product;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.product.Favorite;

@Repository
public interface FavoriteRepository extends JpaRepository<Favorite, String>{
	
	@Query(value = "SELECT * FROM favorite t WHERE (t.user_id=:userId AND t.product_id=:productId)", nativeQuery = true)
	Favorite findByProductUser(@Param("userId") String userId, @Param("productId") String productId);

	@Query(value = "SELECT * FROM favorite t WHERE (t.user_id=:id) ORDER BY t.created_at ASC ", nativeQuery = true)
	List<Favorite> getByUser(@Param("id") String id);
	
	@Query(value = "SELECT count(*) FROM favorite t WHERE t.product_id=:id ", nativeQuery = true)
	Integer cntProductFavorite(@Param("id") String id);
}
