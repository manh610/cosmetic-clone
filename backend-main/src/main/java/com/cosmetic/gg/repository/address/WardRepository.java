package com.cosmetic.gg.repository.address;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.address.Ward;

@Repository
public interface WardRepository extends JpaRepository<Ward, String>{

	@Query(value = "SELECT * FROM wards t WHERE " +
			"t.district_id = :districtId " +
			"ORDER BY t.name ASC", nativeQuery = true)
	List<Ward> findWards(@Param("districtId") String districtId);
}
