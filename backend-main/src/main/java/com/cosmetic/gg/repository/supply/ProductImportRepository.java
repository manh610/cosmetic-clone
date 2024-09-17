package com.cosmetic.gg.repository.supply;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.supply.ProductImport;

@Repository
public interface ProductImportRepository extends JpaRepository<ProductImport, String>{

	@Query(value = "SELECT * FROM product_import t WHERE t.import_id=:id", nativeQuery = true)
	List<ProductImport> findByImport(@Param("id") String id);
}
